{-# LANGUAGE ViewPatterns #-}
module OriginalSpec where

import           Control.Monad
import           Data.Array.Unboxed
import           Data.Char
import           Dijk
import           Test.Hspec

spec :: Spec
spec = describe "Original Dijkstra tests" $ do
  it "should test the original dijkstra implementation" $ do
    let a1 = dijkArr arr0 0 3
    showDijk a1 3 `shouldBe` "([0,1,2,3],6)"

arr0 :: UArray (Int, Int) Int
arr0 = parse entry where
 entry =
  "x17x\
  \xx25\
  \xxx1\
  \xxxx"
 parse = parse' 4 4
 parse' w h = array ((0, 0), (h - 1, w - 1)) . parse'' (4 :: Int) 0 0
 parse'' _ _ _ [] = []
 parse'' w r c xs
  | c == w       = parse'' w (r + 1) 0 xs
  | y : ys <- xs = ((r, c), cvt y) : parse'' w r (c + 1) ys
 cvt 'x' = maxBound
 cvt  d  = ord d - ord '0'

showArr :: UArray (Int, Int) Int -> String
showArr arr = entry where
 entry
  | (_, (succ -> h, succ -> w)) <- bounds arr
  = let
   show' d | 0 <= d, d <= 9 = show d | otherwise = "x"
   go r _ | r == h = ""
   go r c | c == w = '\n' : go (r + 1) 0
   go r c = show' (arr ! (r, c)) ++ go r (c + 1)
   in go 0 0

dijkArr :: UArray (Int, Int) Int -> Int -> Int -> Dijk Int
dijkArr arr s t = entry where
 weights = curry (arr !)
 (_, (_, wm1)) = bounds arr
 neighbors :: Int -> ForM_ Int
 neighbors r = forM_ [c | c <- [0..wm1], arr ! (r, c) < maxBound]
 entry = dijk weights neighbors (== t) s

showDijk :: Dijk Int -> Int -> String
showDijk = (show .) . recon

main :: IO ()
main = do
 let a1 = dijkArr arr0 0 3
 putStrLn $ showArr arr0
 print a1
 putStrLn $ showDijk a1 3
