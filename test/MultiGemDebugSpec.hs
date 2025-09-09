module MultiGemDebugSpec (spec) where

import Test.Hspec
import TotM
import Control.Monad.ST.Strict
import Data.Array.ST

spec :: Spec
spec = do
  describe "Multi-gem debugging" $ do
    it "should debug the multiple gems case step by step" $ do
      -- Multiple gems, only need to get one to target
      let cells = [[Gem, Gem, Air], [Air, Air, Air]]
      let target = (1, 2)
      let (board, _) = createBoard cells target
      let gameState = GameState board target
      
      -- Check initial state
      totMIndex board (0, 0) `shouldBe` Gem
      totMIndex board (0, 1) `shouldBe` Gem
      totMIndex board (0, 2) `shouldBe` Air
      totMIndex board (1, 0) `shouldBe` Air
      totMIndex board (1, 1) `shouldBe` Air
      totMIndex board (1, 2) `shouldBe` Air  -- target position
      
      -- Manual check: what happens if we move Down?
      let (newBoard1, outcome1) = runST $ applyGravity TotM.Down target board
      totMIndex newBoard1 (1, 0) `shouldBe` Gem  -- first gem moved down
      totMIndex newBoard1 (1, 1) `shouldBe` Gem  -- second gem moved down  
      totMIndex newBoard1 (0, 0) `shouldBe` Air  -- cleared
      totMIndex newBoard1 (0, 1) `shouldBe` Air  -- cleared
      outcome1 `shouldBe` Running  -- not won yet, gems at (1,0) and (1,1), target at (1,2)
      
      -- Manual check: what happens if we then move Right?  
      let (newBoard2, outcome2) = runST $ applyGravity TotM.Right target newBoard1
      totMIndex newBoard2 (1, 0) `shouldBe` Air   -- first gem moved right and collected
      totMIndex newBoard2 (1, 1) `shouldBe` Air   -- both gems collected in chain reaction
      totMIndex newBoard2 (1, 2) `shouldBe` Air   -- target remains Air
      
      -- Check win condition manually - should be Won since all gems collected
      let finalOutcome = runST $ do
            mutableGame <- thaw (unTotM newBoard2)
            checkOutcome target mutableGame
      finalOutcome `shouldBe` Won  -- all gems collected
      outcome2 `shouldBe` Won
      
      -- Now check if solver can find this
      let result = solve gameState
      case result of
        Nothing -> expectationFailure "Solver should find a solution for multiple gems case"
        Just moves -> do
          length moves `shouldSatisfy` (> 0)
          length moves `shouldSatisfy` (<= 4)  -- reasonable bound