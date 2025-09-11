module ParsingSpec (spec) where

import           Test.Hspec
import           TotM

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
   let
    results = zipWith (parseCell r) [0..] line
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
  describe "Board creation from parsed grids" $ do
    it "should create a board from a simple 2x2 grid" $ do
      let gridLines = [".*", "@#"]
      let (cells, target) = parseGrid gridLines
      cells `shouldBe` [[Air, Air], [Gem, Obs]]
      target `shouldBe` (0, 1)

      -- Test that we can create a board and game state
      let (board, _) = createBoard cells target
      -- Just verify we can get basic info
      getBoardBounds board `shouldBe` ((0, 0), (1, 1))

    it "should create a game state from case0 grid" $ do
      let gridLines = ["@...#@", ".#....", "......", "...#..", "#..@.#", "......", "#*....", "..#..."]
      let (cells, target) = parseGrid gridLines
      length cells `shouldBe` 8  -- 8 rows
      length (head cells) `shouldBe` 6  -- 6 columns
      target `shouldBe` (6, 1)  -- target position

      -- Test that we can create a board and it has the right bounds
      let (board, _) = createBoard cells target
      getBoardBounds board `shouldBe` ((0, 0), (7, 5))

      -- Test that we can query specific cells
      getCell board (0, 0) `shouldBe` Gem  -- first cell should be gem
      getCell board (6, 1) `shouldBe` Air  -- target should be air