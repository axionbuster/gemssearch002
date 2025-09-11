module SolveTotM2 (solve) where

import           Control.Monad
import           Data.Array.Unboxed
import           Dijk
import           TotM2

-- | Solve the game
solve :: UArray (Int, Int) Cell -> (Int, Int) -> Maybe ([Game], [Direction])
solve board target = do
 let
  (_, (h, w)) = bounds board
  game0 = buildGame (h + 1) (w + 1) target (board !)
 if _game'gemCnt game0 == 0 then error "solve: gems == 0" else do
  let
   trav :: Either (Exc Game) Game -> ForM_ (Direction, Either (Exc Game) Game)
   trav (Right game) = \f -> do
    let action d = f (d, moveGame game d)
    -- order doesn't matter, since we use the same (pure) game state (game).
    void $ action DirRight
    void $ action DirLeft
    void $ action DirUp
    void $ action DirDown
   trav _ = const $ pure ()
   done (Left Won {}) = True
   done  _            = False
  case dijk (\_ _ -> 1) trav done (Right game0) of
   outcome -> case recon outcome of
    ([], _, _) -> Nothing
    (states, steps, _) -> do
     let toGame (Right g)      = g
         toGame (Left (Won g)) = g
         toGame (Left Lost)    = error "solve: losing game in winning strategy"
     Just (toGame <$> states, steps)
