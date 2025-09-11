import           Control.Monad
import           Data.Array.Unboxed
import           Data.Foldable
import           SolveTotM2
import           Text.Printf
import           TotM2

admitBoard :: (Int, Int) -> String -> Maybe ([Game], [Direction])
admitBoard (h, w) s =
 let
  bounds_ = ((0, 0), (h - 1, w - 1))
  indices_ = range bounds_
  cvtchar = \case '#' -> Obs; '%' -> Bat; '@' -> Gem; _ -> Air
  assocs_ = zip indices_ $ cvtchar <$!> s
  proto = accumArray @UArray (\_ x -> x) Air bounds_ assocs_
  target = case find (\(_, c) -> c == '*') (zip indices_ s) of
   Just (loc, _) -> loc
   Nothing       -> error "admitBoard: target cell not found in user input"
  in solve proto target

forI_ :: (Monad m) => Int -> Int -> (Int -> m a) -> m ()
forI_ s e f = go s where go i = when (i <= e) $ f i >> go (i + 1)
{-# INLINE forI_ #-}

main :: IO ()
main = do
 ncases <- readLn
 forI_ 1 ncases $ \casenum -> do
  printf "Test case %v:\n" casenum
  [h, w] <- map read . words <$> getLine
  ls <- concat <$> replicateM h getLine
  case admitBoard (h, w) ls of
   Just (states, directions) -> error "yes, but i don't know what to do yet"
   Nothing                   -> putStrLn "no"
