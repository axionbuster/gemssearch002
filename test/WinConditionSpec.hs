module WinConditionSpec (spec) where

import Test.Hspec
import TotM
import Control.Monad.ST.Strict
import Data.Array.ST

spec :: Spec
spec = do
  describe "Win condition logic" $ do
    it "should win when no gems left" $ do
      -- Create a simple 2x2 board with no gems (all collected)
      let cells = [[Air, Air], [Air, Air]]
      let target = (1, 1)
      let (board, _) = createBoard cells target
      let outcome = runST $ do
            mutableGame <- thaw (unTotM board)
            checkOutcome target mutableGame
      outcome `shouldBe` Won

    it "should lose when bat is at target" $ do
      -- Create a simple 2x2 board with bat at target
      let cells = [[Air, Air], [Air, Bat]]
      let target = (1, 1)
      let (board, _) = createBoard cells target
      let outcome = runST $ do
            mutableGame <- thaw (unTotM board)
            checkOutcome target mutableGame
      outcome `shouldBe` Lost

    it "should be running when gems remain and target is empty" $ do
      -- Create a board with gems but target empty (as it should always be)
      let cells = [[Gem, Air], [Air, Air]]
      let target = (1, 1)
      let (board, _) = createBoard cells target
      let outcome = runST $ do
            mutableGame <- thaw (unTotM board)
            checkOutcome target mutableGame
      outcome `shouldBe` Running