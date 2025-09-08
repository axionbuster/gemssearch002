module PhysicsSpec (spec) where

import Test.Hspec
import TotM hiding (Left, Right)
import qualified TotM
import Control.Monad.ST.Strict
import Prelude hiding (Left, Right)

spec :: Spec
spec = do
  describe "Gravity physics" $ do
    it "should move gem down with Down gravity" $ do
      -- Create a 3x3 board with gem at top
      let cells = [[Gem, Air, Air], [Air, Air, Air], [Air, Air, Air]]
      let target = (2, 2)
      let (board, _) = createBoard cells target
      let (newBoard, outcome) = runST $ applyGravity TotM.Down target board
      -- Gem should move to bottom row
      totMIndex newBoard (2, 0) `shouldBe` Gem
      totMIndex newBoard (0, 0) `shouldBe` Air
      outcome `shouldBe` Running

    it "should move gem right with Right gravity" $ do
      -- Create a 3x3 board with gem at left
      let cells = [[Gem, Air, Air], [Air, Air, Air], [Air, Air, Air]]
      let target = (2, 2)
      let (board, _) = createBoard cells target
      let (newBoard, outcome) = runST $ applyGravity TotM.Right target board
      -- Gem should move to right column
      totMIndex newBoard (0, 2) `shouldBe` Gem
      totMIndex newBoard (0, 0) `shouldBe` Air
      outcome `shouldBe` Running

    it "should stop gem at obstacle" $ do
      -- Create a board with gem, empty space, then obstacle
      let cells = [[Gem, Air, Obs]]
      let target = (0, 1)
      let (board, _) = createBoard cells target
      let (newBoard, outcome) = runST $ applyGravity TotM.Right target board
      -- Gem should stop before obstacle
      totMIndex newBoard (0, 1) `shouldBe` Gem
      totMIndex newBoard (0, 0) `shouldBe` Air
      totMIndex newBoard (0, 2) `shouldBe` Obs
      outcome `shouldBe` Running

    it "should push gems in chain" $ do
      -- Create a board with two gems in a row
      let cells = [[Gem, Gem, Air]]
      let target = (0, 2)
      let (board, _) = createBoard cells target
      let (newBoard, outcome) = runST $ applyGravity TotM.Right target board
      -- Both gems should move right
      totMIndex newBoard (0, 0) `shouldBe` Air
      totMIndex newBoard (0, 1) `shouldBe` Gem
      totMIndex newBoard (0, 2) `shouldBe` Gem
      outcome `shouldBe` Won  -- gem reached target

    it "should handle gems falling out of bounds" $ do
      -- Create a 2x2 board with gem at bottom edge
      let cells = [[Air, Air], [Gem, Air]]
      let target = (0, 0)
      let (board, _) = createBoard cells target
      let (newBoard, outcome) = runST $ applyGravity TotM.Down target board
      -- Gem should stay where it is (can't fall further)
      totMIndex newBoard (1, 0) `shouldBe` Gem
      outcome `shouldBe` Running

    it "should detect win when gem reaches target" $ do
      -- Create a simple case where gem can reach target in one move
      let cells = [[Gem, Air]]
      let target = (0, 1)
      let (board, _) = createBoard cells target
      let (newBoard, outcome) = runST $ applyGravity TotM.Right target board
      totMIndex newBoard (0, 1) `shouldBe` Gem
      outcome `shouldBe` Won

    it "should detect loss when bat reaches target" $ do
      -- Create a case where bat reaches target
      let cells = [[Bat, Air]]
      let target = (0, 1)
      let (board, _) = createBoard cells target
      let (newBoard, outcome) = runST $ applyGravity TotM.Right target board
      totMIndex newBoard (0, 1) `shouldBe` Bat
      outcome `shouldBe` Lost