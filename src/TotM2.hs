{-# LANGUAGE PatternSynonyms #-}
module TotM2
 ( Game(..), IA(..), Direction(..), Exc(..), Cell, pattern Air
 , pattern Bat, pattern Gem, pattern Obs
 , moveGame
 , SA(..)
 , w2cell, cell2w, readSA, writeSA
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
import           Data.Hashable
import           GHC.Generics
import           TotM                    (Cell, pattern Air, pattern Bat,
                                          pattern Gem, pattern Obs)

quotRem4 :: Int -> (Int, Int)
quotRem4 n = (n .>>. 2, n .&. 0b11)

newtype IA = IA (UArray (Int, Int) Word) deriving newtype (Eq, Ord)

instance Hashable IA where
 hashWithSalt salt ~(IA (UArray _ _ _ ba)) = hashWithSalt salt (ByteArray ba)
 {-# INLINE hashWithSalt #-}

newtype SA s = SA (STUArray s (Int, Int) Word)

w2cell :: Word -> Cell
w2cell = fromIntegral

cell2w :: Cell -> Word
cell2w = fromIntegral

readSA :: SA s -> (Int, Int) -> ST s Cell
readSA (SA sa) (r, c) = case quotRem4 c of
 (cq, cr) -> (\w -> w2cell $ (w .>>. cr) .&. 0b11) <$!> readArray sa (r, cq)
{-# INLINE readSA #-}

writeSA :: SA s -> (Int, Int) -> Cell -> ST s ()
writeSA (SA s) (r, c) v = case quotRem4 c of
 (cq, cr) -> do
  w1 <- readArray s (r, cq)
  let w2 = w1 .&. complement (0b11 .<<. cr)
      w3 = w2 .|. (cell2w v .<<. cr)
  writeArray s (r, cq) w3
{-# iNLINE writeSA #-}

newtype IntRef s = IR (STUArray s Int Int)
newIR :: Int -> ST s (IntRef s)
newIR = fmap IR . newArray (0, 0)
readIR :: MArray (STUArray s) Int m => IntRef s -> m Int
readIR (IR x) = readArray x 0
writeIR :: MArray (STUArray s) Int m => IntRef s -> Int -> m ()
writeIR (IR x) = writeArray x 0

data Game = Game IA (Int, Int) Int -- board, target, gem count (cached)
 deriving (Eq, Ord, Generic)
instance Hashable Game
data MGame s =
 MGame
  (SA s) -- board
  (Int, Int) -- target
  (Int, Int) -- last allowed row and column; (height - 1, width - 1)
  (IntRef s) -- gem count (cached)
-- | If we won, we wanna see the outcome
data Exc a = Won a | Lost deriving (Eq, Ord, Generic)
instance Hashable a => Hashable (Exc a)

type Next = (Int, Int) -> (Int, Int)

movable :: Cell -> Bool
movable = \case Gem -> True; Bat -> True; _ -> False

-- try to move the piece - last parameter indicates its location
chain :: MGame s -> Next -> (Int, Int) -> ExceptT (Exc (MGame s)) (ST s) ()
chain game@(MGame board target (hm1, wm1) ngems) next = go0 where
 locationGood = inRange ((0, 0), (hm1, wm1))
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
      else lift (writeIR ngems 0) >> (throwError $! Won game)
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
     when (movable that') $ go0 here -- if it did, try again.
{-# INLINE chain #-}

data Direction = DirUp | DirDown | DirLeft | DirRight
 deriving (Show, Eq)

-- GHC's ability to optimize away the one-shot list in `forM_ [1..123456] ..`
-- is still unreliable, as of GHC 9.12.2, so we express our loop directly.
forI0_ :: (Ord t, Monad f, Num t) => t -> (t -> f a) -> f ()
forI0_ e f = go 0 where go i = when (i <= e) $ f i >> go (i + 1)

moveGameTmpl :: Game -> Next -> Either (Exc Game) Game
moveGameTmpl game@(Game (IA board) target gems) next
 | gems < 1 = Right game
 | otherwise = runST $ do
  let upperBounds = snd $ bounds board
  mboard <- SA <$> thaw board
  mgems <- newIR gems
  res <- runExceptT $ do
   forI0_ (fst upperBounds) $ \r -> do
    forI0_ (snd upperBounds) $ \c -> do
     chain (MGame mboard target upperBounds mgems) next (r, c)
  let
   freezeGame (MGame mb _ _ mg) = do
    board' <- IA <$!> case mb of SA m -> unsafeFreeze m
    gems' <- readIR mg
    pure $! Game board' target gems'
  case res of
   Left Lost -> pure $ Left Lost
   Left (Won g) -> Right <$!> freezeGame g
   Right () -> do
    board' <- IA <$!> case mboard of SA mb -> unsafeFreeze mb
    gems' <- readIR mgems
    let game' = Game board' target gems'
    pure $ if gems' == 0
    then Left (Won game')
    else Right $! Game board' target gems'
{-# INLINE moveGameTmpl #-}

moveGame :: Game -> Direction -> Either (Exc Game) Game
moveGame game = \case
 DirUp ->    moveGameTmpl game $ first (subtract 1)
 DirDown ->  moveGameTmpl game $ first (+ 1)
 DirLeft ->  moveGameTmpl game $ second (subtract 1)
 DirRight -> moveGameTmpl game $ second (+ 1)
