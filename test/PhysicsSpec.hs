module PhysicsSpec (spec) where

import           Test.Hspec
import           TotM

spec :: Spec
spec = do
  describe "stepGame API" $ do
    it "should detect bat hitting target" $ do
      -- Simple test: bat should move to target and cause loss
      let cells = [[Air, Bat], [Air, Air]]  -- bat at (0,1), target at (1,1)
      let target = (1, 1)
      let (board, _) = createBoard cells target
      let gameState = mkGameState board target
      case stepGame DirDown gameState of
        Left BatHitTarget -> return ()  -- Expected: bat hits target
        Left AllGemsCollected -> expectationFailure "Expected BatHitTarget but got AllGemsCollected"
        Right _ -> expectationFailure "Expected BatHitTarget but game continued"

    it "should detect all gems collected" $ do
      -- Single gem that moves directly to target
      let cells = [[Gem, Air]]  -- gem at (0,0), target at (0,1)
      let target = (0, 1)
      let (board, _) = createBoard cells target
      let gameState = mkGameState board target
      case stepGame DirRight gameState of
        Left AllGemsCollected -> return ()  -- Expected: gem collected
        Left BatHitTarget -> expectationFailure "Expected AllGemsCollected but got BatHitTarget"
        Right _ -> expectationFailure "Expected AllGemsCollected but game continued"

    it "should continue game when no win/loss condition" $ do
      -- Gem moves but doesn't hit target (stopped by obstacle)
      let cells = [[Gem, Obs, Air]]  -- gem at (0,0), obstacle at (0,1), target at (0,2)
      let target = (0, 2)
      let (board, _) = createBoard cells target
      let gameState = mkGameState board target
      case stepGame DirDown gameState of
        Left _ -> expectationFailure "Expected game to continue but got exception"
        Right newState -> do
          -- Game should continue - we can verify the board changed
          gsBoard newState `shouldNotBe` gsBoard gameState
