module ParsingSpec (spec) where

import Test.Hspec
import TotM
import qualified Data.Array.Unboxed as A

-- Test helper functions
parseGrid :: [String] -> ([[Cell]], (Int, Int))
parseGrid gridLines =
 let -- Convert each line to cells and collect target positions
     results = zipWith parseRow [0..] gridLines
     cellRows = map fst results
     allTargets = concatMap snd results
     target = head allTargets  -- assume exactly one target
 in (cellRows, target)
 where
  parseRow r line =
   let results = zipWith (parseCell r) [0..] line
       rowCells = map fst results
       rowTargets = concatMap snd results
   in (rowCells, rowTargets)

  parseCell r c char
   | char == '*' = (Air, [(r, c)])
   | char == '.' = (Air, [])
   | char == '#' = (Obs, [])
   | char == '%' = (Bat, [])
   | char == '@' = (Gem, [])
   | otherwise = (Air, [])

spec :: Spec
spec = do
  describe "Grid parsing" $ do
    it "should parse a simple 2x2 grid correctly" $ do
      let gridLines = [".*", "@#"]
      let (cells, target) = parseGrid gridLines
      cells `shouldBe` [[Air, Air], [Gem, Obs]]
      target `shouldBe` (0, 1)

    it "should parse the case0 grid correctly" $ do
      let gridLines = ["@...#@", ".#....", "......", "...#..", "#..@.#", "......", "#*....", "..#..."]
      let (cells, target) = parseGrid gridLines
      length cells `shouldBe` 8  -- 8 rows
      length (head cells) `shouldBe` 6  -- 6 columns
      target `shouldBe` (6, 1)  -- target position

    it "should count gems correctly in case0" $ do
      let gridLines = ["@...#@", ".#....", "......", "...#..", "#..@.#", "......", "#*....", "..#..."]
      let (cells, target) = parseGrid gridLines
      let (board, _) = createBoard cells target
      let gemCount = countGemsInBoard board
      gemCount `shouldBe` 3  -- should have 3 gems

-- Helper function to count gems in a board
countGemsInBoard :: TotM -> Int
countGemsInBoard board = 
  let ((r1, c1), (r2, c2)) = totMBounds board
      coords = [(r, c) | r <- [r1..r2], c <- [c1..c2]]
      cells = [totMIndex board coord | coord <- coords]
  in length $ filter (== Gem) cells