module SolveTotM2 (solve) where

import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Array.ST
import           Data.Array.Unboxed
import           Dijk
import           TotM2

solve :: UArray (Int, Int) Cell -> (Int, Int) -> Maybe ([Game], [Direction])
solve board target = do
 let
  forI0_ e f = go 0 where go i = when (i <= e) $ f i >> go (i + 1)
  (_, (hm1, wm1)) = bounds board
  gems = (`execState` 0) $ do
   forI0_ hm1 $ \r -> forI0_ wm1 $ \c ->
    when (board ! (r, c) /= 0) $ get >>= put . (+ 1)
 if gems == 0 then error "gems == 0" else do
  let
   compacted = runSTUArray $ do
    sa <- newArray ((0, 0), (hm1, 4 * (wm1 + 1) - 1)) 0
    forI0_ hm1 $ \r ->
     forI0_ wm1 $ \c ->
      writeSA (SA sa) (r, c) (board ! (r, c))
    pure sa
  let game0 = Game (IA compacted) target gems
  let
   trav :: Either (Exc Game) Game -> ForM_ (Direction, Either (Exc Game) Game)
   trav (Right game) f = do
    let action d = f (d, moveGame game d)
    void $ action DirRight
    void $ action DirLeft
    void $ action DirLeft
    void $ action DirUp
   trav _ _ = pure ()
   done (Left Won {}) = True
   done  _            = False
  case dijk (\_ _ -> 1) trav done (Right game0) of
   outcome -> case recon outcome of
    ([], _, _) -> Nothing
    (states, steps, _) -> do
     let toGame (Right g)      = g
         toGame (Left (Won g)) = g
         toGame (Left Lost)    = error "!!!"
     Just (toGame <$!> states, steps)
