module IntegrationSpec (spec) where

import           Data.Maybe
import           Test.Hspec
import           TotM

spec :: Spec
spec = do
 describe "Integration tests" $ do
  it "should solve trivial single gem case" $ do
   -- Single gem that can slide right to target
   let cells = [[Gem, Air]]
   let target = (0, 1)
   let (board, _) = createBoard cells target
   let gameState = mkGameState board target
   let result = solve gameState
   result `shouldSatisfy` isJust

  it "should solve simple two-move case" $ do
   -- Gem needs to move down then right
   let cells = [[Gem, Air], [Air, Air]]
   let target = (1, 1)
   let (board, _) = createBoard cells target
   let gameState = mkGameState board target
   let result = solve gameState
   result `shouldSatisfy` isJust

  it "should return Nothing for impossible case" $ do
   -- Gem completely blocked by obstacles
   let cells = [[Gem, Obs], [Obs, Air]]
   let target = (1, 1)
   let (board, _) = createBoard cells target
   let gameState = mkGameState board target
   let result = solve gameState
   result `shouldBe` Nothing

  it "should handle multiple gems case" $ do
   -- Multiple gems, solver should find a way to collect them
   let cells = [[Gem, Gem, Air], [Air, Air, Air]]
   let target = (1, 2)
   let (board, _) = createBoard cells target
   let gameState = mkGameState board target
   let result = solve gameState
   result `shouldSatisfy` isJust
