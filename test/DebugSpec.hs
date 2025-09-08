module DebugSpec (spec) where

import Test.Hspec
import TotM
import Control.Monad.ST.Strict

spec :: Spec
spec = do
  describe "Debug failing tests" $ do
    it "should debug the obstacle stop test" $ do
      -- Create a board with gem, empty space, then obstacle
      let cells = [[Gem, Air, Obs]]
      let target = (0, 1)
      let (board, _) = createBoard cells target
      
      -- Check initial state
      totMIndex board (0, 0) `shouldBe` Gem
      totMIndex board (0, 1) `shouldBe` Air
      totMIndex board (0, 2) `shouldBe` Obs
      
      let (newBoard, outcome) = runST $ applyGravity TotM.Right target board
      
      -- Debug: check final positions
      let finalGemAt0 = totMIndex newBoard (0, 0)
      let finalGemAt1 = totMIndex newBoard (0, 1) 
      let finalGemAt2 = totMIndex newBoard (0, 2)
      
      -- Print debug info
      finalGemAt0 `shouldBe` Air  -- gem should have moved
      finalGemAt1 `shouldBe` Gem  -- gem should be at target
      finalGemAt2 `shouldBe` Obs  -- obstacle unchanged
      
      -- Since there's only 1 gem and it's at the target, this should be Won
      outcome `shouldBe` Won

    it "should debug the chain push test" $ do
      -- Create a board with two gems in a row
      let cells = [[Gem, Gem, Air]]
      let target = (0, 2)
      let (board, _) = createBoard cells target
      
      let (newBoard, outcome) = runST $ applyGravity TotM.Right target board
      
      -- Debug: check final positions
      let finalAt0 = totMIndex newBoard (0, 0)
      let finalAt1 = totMIndex newBoard (0, 1)
      let finalAt2 = totMIndex newBoard (0, 2)
      
      finalAt0 `shouldBe` Air   -- first position should be empty
      finalAt1 `shouldBe` Gem   -- first gem moved here
      finalAt2 `shouldBe` Gem   -- second gem at target
      
      -- Since there are 2 gems total and 1 is at target, this should be Running
      outcome `shouldBe` Running

    it "should debug a trivial solver case" $ do
      -- Single gem that can slide right to target
      let cells = [[Gem, Air]]
      let target = (0, 1)
      let (board, _) = createBoard cells target
      let gameState = GameState board target
      
      -- Check that manual physics works
      let (newBoard, outcome) = runST $ applyGravity TotM.Right target board
      totMIndex newBoard (0, 1) `shouldBe` Gem
      outcome `shouldBe` Won
      
      -- Now check solver
      let result = solve gameState
      result `shouldSatisfy` (\x -> case x of Just _ -> True; Nothing -> False)
      case result of
        Nothing -> expectationFailure "Solver returned Nothing for trivial case"
        Just directions -> directions `shouldBe` [TotM.Right]