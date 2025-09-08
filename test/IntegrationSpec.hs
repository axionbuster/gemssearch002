module IntegrationSpec (spec) where

import Test.Hspec
import TotM

spec :: Spec
spec = do
  describe "Integration tests" $ do
    it "should solve trivial single gem case" $ do
      -- Single gem that can slide right to target
      let cells = [[Gem, Air]]
      let target = (0, 1)
      let (board, _) = createBoard cells target
      let gameState = GameState board target
      let result = solve gameState
      result `shouldSatisfy` (\x -> case x of Just _ -> True; Nothing -> False)

    it "should solve simple two-move case" $ do
      -- Gem needs to move down then right
      let cells = [[Gem, Air], [Air, Air]]
      let target = (1, 1)
      let (board, _) = createBoard cells target
      let gameState = GameState board target
      let result = solve gameState
      result `shouldSatisfy` (\x -> case x of Just _ -> True; Nothing -> False)

    it "should return Nothing for impossible case" $ do
      -- Gem completely blocked by obstacles
      let cells = [[Gem, Obs], [Obs, Air]]
      let target = (1, 1)
      let (board, _) = createBoard cells target
      let gameState = GameState board target
      let result = solve gameState
      result `shouldBe` Nothing

    it "should handle multiple gems case" $ do
      -- Multiple gems, only need to get one to target
      let cells = [[Gem, Gem, Air], [Air, Air, Air]]
      let target = (1, 2)
      let (board, _) = createBoard cells target
      let gameState = GameState board target
      let result = solve gameState
      result `shouldSatisfy` (\x -> case x of Just _ -> True; Nothing -> False)

    it "should solve the case0 scenario" $ do
      -- Test the actual case0 input
      let gridLines = ["@...#@", ".#....", "......", "...#..", "#..@.#", "......", "#*....", "..#..."]
      let (cells, target) = parseGridForTest gridLines
      let (board, _) = createBoard cells target
      let gameState = GameState board target
      let result = solve gameState
      -- This should find a solution if the solver is working correctly
      case result of
        Nothing -> expectationFailure "case0 should be solvable but returned Nothing"
        Just directions -> do
          length directions `shouldSatisfy` (> 0)
          length directions `shouldSatisfy` (< 100)  -- reasonable bound

-- Helper function to parse grid (copied from Main.hs logic)
parseGridForTest :: [String] -> ([[Cell]], (Int, Int))
parseGridForTest gridLines =
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