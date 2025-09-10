module MultiGemDebugSpec (spec) where

import Test.Hspec
import TotM

spec :: Spec
spec = do
  describe "Multi-gem debugging with stepGame API" $ do
    it "should handle multiple gems step by step" $ do
      -- Multiple gems, both can be collected
      let cells = [[Gem, Gem, Air], [Air, Air, Air]]
      let target = (1, 2)
      let (board, _) = createBoard cells target
      let gameState = mkGameState board target
      
      -- Step 1: Move Down (gems should move down)
      case stepGame DirDown gameState of
        Left _ -> expectationFailure "Step 1 should not end game"
        Right gameState1 -> do
          -- Step 2: Move Right (gems should slide to target and be collected)
          case stepGame DirRight gameState1 of
            Left AllGemsCollected -> return ()  -- Expected: all gems collected
            Left BatHitTarget -> expectationFailure "Unexpected BatHitTarget"
            Right _ -> expectationFailure "Expected AllGemsCollected after step 2"
      
    it "should solve multi-gem puzzle" $ do
      -- Test that solver can handle multiple gems
      let cells = [[Gem, Gem, Air], [Air, Air, Air]]
      let target = (1, 2)
      let (board, _) = createBoard cells target
      let gameState = mkGameState board target
      
      let result = solve gameState
      case result of
        Nothing -> expectationFailure "Solver should find a solution for multiple gems case"
        Just directions -> length directions `shouldSatisfy` (> 0)