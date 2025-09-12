{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module TotM2.MovementSpec (spec) where
import           Control.Monad
import           Data.Array.Unboxed
import           Data.Function
import qualified Data.List          as List
import           Test.Hspec
import           TotM2

showArray1 :: (Int, Int) -> (Int, Int) -> UArray (Int, Int) Cell -> String
showArray1 (h, w) target arr1 = do
 let ran0 = range ((0, 0), (h - 1, w - 1)) & List.groupBy ((==) `on` fst)
 let unchar Air = '.'; unchar Gem = '@'; unchar Bat = '%'; unchar Obs = '#'
 let unchar' rc | rc == target = '*' | otherwise = unchar (arr1 ! rc)
 let lines_ = map (map unchar') ran0
 List.intercalate "\n" lines_

showGame :: Game -> String
showGame game@(Game _ target _ height width) =
 showArray1 (height, width) target $ teardownGameBoard game

admitBoard :: (Int, Int) -> String -> Game
admitBoard (h, w) s =
 let
  bounds_ = ((0, 0), (h - 1, w - 1))
  indices_ = range bounds_
  cvtchar = \case '#' -> Obs; '%' -> Bat; '@' -> Gem; _ -> Air
  assocs_ = zip indices_ $ cvtchar <$!> s
  proto = accumArray @UArray (\_ x -> x) Air bounds_ assocs_
  target = case List.find (\(_, c) -> c == '*') (zip indices_ s) of
   Just (loc, _) -> loc
   Nothing       -> error "admitBoard: target cell not found in user input"
  in buildGame h w target (proto !)

newtype ShowGame = ShowGame Game deriving (Eq)
instance Show ShowGame where
 show (ShowGame g) = showGame g

spec :: Spec
spec = describe "TotM2.MovementSpec" $ do
 it "should not cause walls to move" $ do
  -- common array
  let game = admitBoard (3, 5) $ concat ["@@@@.", "####.", "....*"]
  let Right down = moveGame game DirDown
  ShowGame down `shouldBe` ShowGame game
