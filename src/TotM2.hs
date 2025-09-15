{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : TotM2
-- Description : Bit-packed implementation of the Gem Seeker minigame solver
-- Copyright   : (c) 2025, axionbuster
-- License     : BSD-3-Clause
-- Maintainer  : axionbuster
--
-- This module provides the core data types and game mechanics for a solver for
-- the Gem Seeker minigame from Tomb of the Mask. It features a bit-packed
-- representation of the game board for memory efficiency, where each cell is
-- encoded using two bits. This design is optimized for performance-critical
-- scenarios, such as pathfinding in large game states.
--
-- The main data structure, 'Game', encapsulates the entire state of the game,
-- including the board, target location, and counts of gems and bats. Game
-- state transitions are handled by 'moveGame', which simulates the effect of
-- gravity in one of the four cardinal directions.
module TotM2
 ( Game(..), Direction(..), Exc(..), IA, Cell
 , pattern Air, pattern Bat, pattern Gem, pattern Obs
 , moveGame
 , buildGame
 , teardownGameBoard
 ) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.ST.Strict
import           Control.Monad.Trans
import           Data.Array.Base
import           Data.Array.Byte
import           Data.Array.ST
import           Data.Bifunctor
import           Data.Bits
import           Data.Functor
import           Data.Hashable
import           Data.Word
import           GHC.Generics

-- | We use two bits to encode four possibilities for each block: air (0),
-- bat (1), gem (2), and obstacle (wall; 3). Functions that say they take
-- 'Cell's require that all bits but the least significant two bits are
-- zero, and any function that outputs a 'Cell' or 'Cell's will make sure
-- that no value greater than 3 will be produced. See the bundled pattern
-- synonyms 'Air', 'Bat', 'Gem', and 'Obs' - they are marked @COMPLETE@
-- for convenience in pattern matching.
type Cell = Word8

pattern Air, Bat, Gem, Obs :: Cell
-- | 0
pattern Air = 0b00
-- | 1
pattern Bat = 0b01
-- | 2
pattern Gem = 0b10
-- | 3
pattern Obs = 0b11
{-# COMPLETE Air, Bat, Gem, Obs #-}

-- | Immutable array, two-bit encoded. This is internally a bit-packed array.
-- Each row is made up of words that each contain up to 16 or 32 pairs of
-- bits, as compactly as possible, from the least significant words, and
-- from the least significant bits. No compression is done /across/ the rows.
--
-- NOTE: 'IA' is a private type.
newtype IA = IA (UArray (Int, Int) Word) deriving newtype (Eq, Ord)

-- | Only the internal 'ByteArray' is used for hashing.
instance Hashable IA where
 hashWithSalt salt (IA (UArray _ _ _ ba)) = hashWithSalt salt (ByteArray ba)

-- | Stateful array, bit-packed. See 'IA' for the layout information.
newtype SA s = SA (STUArray s (Int, Int) Word)

w2cell :: Word -> Cell
w2cell = fromIntegral

cell2w :: Cell -> Word
cell2w = fromIntegral

readSA :: SA s -> (Int, Int) -> ST s Cell
readSA (SA sa) (r, c) = case quotRemWord c of
 (cq, cr) -> readArray sa (r, cq) <&> \w -> w2cell ((w .>>. (2 * cr)) .&. 0b11)

writeSA :: SA s -> (Int, Int) -> Cell -> ST s ()
writeSA (SA s) (r, c) v = case quotRemWord c of
 (cq, cr) -> do
  w1 <- readArray s (r, cq)
  let w2 = w1 .&. complement (0b11 .<<. (2 * cr))
      w3 = w2 .|. (cell2w v .<<. (2 * cr))
  writeArray s (r, cq) w3

-- we take into account the native word size
quotRemWord :: Int -> (Int, Int)
quotRemWord n = case finiteBitSize (0 :: Word) of
 -- two bits each
 64 -> (n .>>. 5, n .&. 0b11111) -- 32
 32 -> (n .>>. 4, n .&.  0b1111) -- 16
 _  -> error "quotRemWord: unknown finiteBitSize of Word"

-- we use a mutable unboxed array instead of a MutVar# (~ IORef, STRef _)
-- because the latter is boxed.
newtype IntRef s = IR (STUArray s Int Int)
newIR :: Int -> ST s (IntRef s)
newIR = fmap IR . newArray (0, 0)
readIR :: MArray (STUArray s) Int m => IntRef s -> m Int
readIR (IR x) = readArray x 0
writeIR :: MArray (STUArray s) Int m => IntRef s -> Int -> m ()
writeIR (IR x) = writeArray x 0

-- | Immutable game
data Game = Game
 { _game'board  :: IA
 , _game'target :: (Int, Int)
 , _game'gemCnt :: Int
 , _game'height :: Int
 , _game'width  :: Int
 }
 deriving (Eq, Ord, Generic)
instance Hashable Game
data MGame s =
 MGame
  (SA s) -- board
  (Int, Int) -- target
  Int -- height
  Int -- width
  (IntRef s) -- gem count

-- | Game over. Either we win and know the final board state, or we lose.
data Exc a = Won a | Lost deriving (Eq, Ord, Generic)
instance Hashable a => Hashable (Exc a)

-- | @buildGame h w target cellAt@ will build the game with a height of
-- @h@, width of @w@, a target cell (must be 'Air') at @target@, and
-- with a pure accessor function of @cellAt@.
buildGame :: Int -> Int -> (Int, Int) -> ((Int, Int) -> Cell) -> Game
buildGame h w target cellAt =
 let
  wWords = case quotRemWord w of (wq, wr) -> wq + fromEnum (wr /= 0)
  bounds' = ((0, 0), (h - 1, wWords - 1))
  (arr, gems) = runST $ do
   sa <- newArray bounds' 0
   gemsRef <- newIR 0
   forI0_ (h - 1) $ \r ->
    forI0_ (w - 1) $ \c -> do
     let cell = cellAt (r, c)
     when (cell == Gem) $ do
      g <- readIR gemsRef
      writeIR gemsRef (g + 1)
     writeSA (SA sa) (r, c) cell
   frozenArr <- unsafeFreeze sa
   finalGems <- readIR gemsRef
   pure (IA frozenArr, finalGems)
 in Game arr target gems h w

-- | Bit-expand the 2-bit-per-cell-encoded board.
teardownGameBoard :: Game -> UArray (Int, Int) Cell
teardownGameBoard (Game (IA board) _ _ height width) = do
 runSTUArray $ do
  arr1 <- newArray ((0, 0), (height - 1, width - 1)) 0
  forI0_ (height - 1) $ \r -> forI0_ (width - 1) $ \c -> do
   -- we don't have the equivalent of `readSA` for IA, so we improvise.
   let (q, re) = quotRemWord c
   let b = fromIntegral $ ((board ! (r, q)) .>>. (2 * re)) .&. 0b11
   writeArray arr1 (r, c) b
  pure arr1

type Next = (Int, Int) -> (Int, Int)

movable :: Cell -> Bool
movable = \case Gem -> True; Bat -> True; _ -> False

-- try to move the piece - last parameter indicates its location
chain :: MGame s -> Next -> (Int, Int) -> ExceptT (Exc (MGame s)) (ST s) ()
chain game@(MGame board target h w ngems) next = go0 where
 locationGood = inRange ((0, 0), (h - 1, w - 1))
 go0 (r, c) = when (locationGood (r, c)) $ do
  let here = (r, c)
  this <- lift $ readSA board here
  let there = next here
  when (movable this && locationGood there) $ do
    that <- lift $ readSA board there
    case that of
     Air | there == target -> case this of
      Bat -> throwError Lost
      _   -> do
       gems <- lift $ readIR ngems
       if gems > 1
       then lift $ do
        writeIR ngems (gems - 1)
        writeSA board here Air
       else do
        lift $ writeIR ngems 0
        lift $ writeSA board here Air
        throwError $ Won game
     Air -> do
      lift $ do
       writeSA board there this
       writeSA board here Air
      go0 there
     Obs -> pure ()
     _ -> do
      -- movable obstacle ahead: push that object to clear space and then retry.
      -- the depth first traversal here implicitly topologically sorts
      -- the movables, which is why 'chain' can be called in any order.
      chain game next there -- *** recursion! try to move that thing.
      that' <- lift $ readSA board there -- did it move?
      unless (movable that') $ go0 here -- if it did, try again.

-- | Up, down, left, or right
data Direction = DirUp | DirDown | DirLeft | DirRight
 deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance Hashable Direction

-- GHC's ability to optimize away the one-shot list in `forM_ [1..123456] ..`
-- is still unreliable, as of GHC 9.12.2, so we express our loop directly.
forI0_ :: (Ord t, Monad f, Num t) => t -> (t -> f a) -> f ()
forI0_ e f = go 0 where go i = when (i <= e) $ f i >> go (i + 1)

moveGameTmpl :: Game -> Next -> Either (Exc Game) Game
moveGameTmpl game@(Game (IA board) target gems h w) next
 | gems < 1 = Left (Won game)
 | otherwise = runST $ do
  mboard <- SA <$> thaw board
  mgems <- newIR gems
  let mgame = MGame mboard target h w mgems
  res <- runExceptT $ forI0_ (h - 1) $ \r -> forI0_ (w - 1) $ \c ->
   chain mgame next (r, c)
  let
   freezeGame (MGame mb _ h' w' mg) = do
    board' <- case mb of SA m -> IA <$> unsafeFreeze m
    gems' <- readIR mg
    pure $ Game board' target gems' h' w'
  case res of
   Left Lost -> pure $ Left Lost
   Left (Won g) -> Left . Won <$> freezeGame g
   Right () -> do
    game' <- freezeGame mgame
    pure $! if _game'gemCnt game' == 0
    then Left (Won game')
    else Right game'

-- | Change the direction of gravity, causing movable pieces
-- (i.e., 'Gem' and 'Bat') to move.
--
-- If this function returns a 'Left' value, the game is over. Otherwise
-- ('Right'), the game is ongoing.
moveGame :: Game -> Direction -> Either (Exc Game) Game
moveGame game = \case
 DirUp ->    moveGameTmpl game $ first  (subtract 1)
 DirDown ->  moveGameTmpl game $ first  (+ 1)
 DirLeft ->  moveGameTmpl game $ second (subtract 1)
 DirRight -> moveGameTmpl game $ second (+ 1)
