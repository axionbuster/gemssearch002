module DebugSpec (spec) where

import           Test.Hspec
import           TotM

spec :: Spec
spec = do
  describe "Debug game mechanics with stepGame API" $ do
    it "should collect gem when it slides into target" $ do
      -- Create a board with gem sliding into target position
      let cells = [[Gem, Air, Obs]]
      let target = (0, 1)
      let (board, _) = createBoard cells target
      let gameState = mkGameState board target

      case stepGame DirRight gameState of
        Left AllGemsCollected -> return ()  -- Expected: gem collected
        Left BatHitTarget -> expectationFailure "Unexpected BatHitTarget"
        Right _ -> expectationFailure "Expected AllGemsCollected but game continued"

    it "should collect multiple gems in chain reaction" $ do
      -- Create a board with two gems sliding into target
      let cells = [[Gem, Gem, Air]]
      let target = (0, 2)
      let (board, _) = createBoard cells target
      let gameState = mkGameState board target

      case stepGame DirRight gameState of
        Left AllGemsCollected -> return ()  -- Expected: all gems collected
        Left BatHitTarget -> expectationFailure "Unexpected BatHitTarget"
        Right _ -> expectationFailure "Expected AllGemsCollected but game continued"

    it "should solve trivial single gem case" $ do
      -- Single gem that can slide right to target
      let cells = [[Gem, Air]]
      let target = (0, 1)
      let (board, _) = createBoard cells target
      let gameState = mkGameState board target

      -- Test solver
      let result = solve gameState
      result `shouldSatisfy` (\case Just _ -> True; Nothing -> False)
      case result of
        Nothing -> expectationFailure "Solver returned Nothing for trivial case"
        Just directions -> directions `shouldBe` [DirRight]