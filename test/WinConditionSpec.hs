module WinConditionSpec (spec) where

import           Test.Hspec
import           TotM

spec :: Spec
spec = do
  describe "Win/Loss conditions via stepGame" $ do
    it "should detect win when all gems collected" $ do
      -- Single gem that moves to target
      let cells = [[Gem, Air]]
      let target = (0, 1)
      let (board, _) = createBoard cells target
      let gameState = mkGameState board target
      case stepGame DirRight gameState of
        Left AllGemsCollected -> return ()  -- Expected
        Left BatHitTarget     -> expectationFailure "Unexpected BatHitTarget"
        Right _               -> expectationFailure "Expected AllGemsCollected"

    it "should detect loss when bat hits target" $ do
      -- Bat that moves to target
      let cells = [[Bat, Air]]
      let target = (0, 1)
      let (board, _) = createBoard cells target
      let gameState = mkGameState board target
      case stepGame DirRight gameState of
        Left BatHitTarget -> return ()  -- Expected
        Left AllGemsCollected -> expectationFailure "Unexpected AllGemsCollected"
        Right _ -> expectationFailure "Expected BatHitTarget"

    it "should continue when no end condition met" $ do
      -- Gem moves but doesn't reach target (blocked by obstacle)
      let cells = [[Gem, Obs, Air]]
      let target = (0, 2)  -- target at end, gem blocked by obstacle
      let (board, _) = createBoard cells target
      let gameState = mkGameState board target
      case stepGame DirRight gameState of
        Left _  -> expectationFailure "Expected game to continue"
        Right _ -> return ()  -- Expected: game continues