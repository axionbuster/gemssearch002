module DebugSpec (spec) where

import Test.Hspec
import TotM
import Control.Monad.ST.Strict

spec :: Spec
spec = do
  describe "Debug gem collection mechanics" $ do
    it "should collect gem when it slides into target" $ do
      -- Create a board with gem sliding into target position
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
      
      -- Gem should disappear when hitting target
      finalGemAt0 `shouldBe` Air  -- gem moved from here
      finalGemAt1 `shouldBe` Air  -- target remains Air (gem disappeared)
      finalGemAt2 `shouldBe` Obs  -- obstacle unchanged
      
      -- Since all gems are collected, game should be won
      outcome `shouldBe` Won

    it "should collect multiple gems in chain reaction" $ do
      -- Create a board with two gems sliding into target
      let cells = [[Gem, Gem, Air]]
      let target = (0, 2)
      let (board, _) = createBoard cells target
      
      let (newBoard, outcome) = runST $ applyGravity TotM.Right target board
      
      -- Debug: check final positions
      let finalAt0 = totMIndex newBoard (0, 0)
      let finalAt1 = totMIndex newBoard (0, 1)
      let finalAt2 = totMIndex newBoard (0, 2)
      
      finalAt0 `shouldBe` Air   -- first gem moved from here
      finalAt1 `shouldBe` Air   -- second gem moved from here  
      finalAt2 `shouldBe` Air   -- target remains Air (both gems collected)
      
      -- Since all gems are collected, game should be won
      outcome `shouldBe` Won

    it "should solve trivial single gem case" $ do
      -- Single gem that can slide right to target
      let cells = [[Gem, Air]]
      let target = (0, 1)
      let (board, _) = createBoard cells target
      let gameState = GameState board target
      
      -- Check that manual physics works
      let (newBoard, outcome) = runST $ applyGravity TotM.Right target board
      totMIndex newBoard (0, 1) `shouldBe` Air  -- gem collected
      outcome `shouldBe` Won
      
      -- Now check solver
      let result = solve gameState
      result `shouldSatisfy` (\x -> case x of Just _ -> True; Nothing -> False)
      case result of
        Nothing -> expectationFailure "Solver returned Nothing for trivial case"
        Just directions -> directions `shouldBe` [TotM.Right]