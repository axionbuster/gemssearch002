{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main (main) where

import           Control.Exception   (evaluate)
import           Control.Monad
import           TotM
import           UnliftIO.Async      (race)
import           UnliftIO.Concurrent (threadDelay)

charToCell :: Char -> Cell
charToCell '.' = Air
charToCell '#' = Obs
charToCell '%' = Bat
charToCell '@' = Gem
charToCell '*' = Air  -- target is air, but we track position separately
charToCell _   = Air  -- fallback

main :: IO ()
main = do
 nStr <- getLine
 let n = read nStr
 mapM_ processTestCase [1..n]

processTestCase :: Int -> IO ()
processTestCase testNum = do
 dimensionsLine <- getLine
 let [_width, height] = map read $ words dimensionsLine

 -- Read the grid (height lines)
 gridLines <- replicateM height getLine

 -- Parse grid and find target
 let (cells, target) = parseGrid gridLines
 let (board, _) = createBoard cells target
 let gameState = mkGameState board target

 putStrLn $ "Test case " ++ show testNum ++ ":"

 -- Race the solver against a 5-second timeout
 result <- race (threadDelay (5 * 1000000)) (evaluate $ solve gameState)
 case result of
  Left _  -> putStrLn "timeout after 5 seconds"
  Right Nothing -> putStrLn "no"
  Right (Just directions) -> do
   putStrLn "yes"
   simulateSteps gameState directions

parseGrid :: [String] -> ([[Cell]], (Int, Int))
parseGrid gridLines =
 let -- Convert each line to cells and collect target positions
     results = zipWith parseRow [0..] gridLines
     cellRows = map fst results
     allTargets = concatMap snd results
     target = case allTargets of
      t : _ -> t
      []    -> error "No target found in grid"
 in (cellRows, target)
 where
  parseRow r line =
   let results = zipWith (parseCell r) [0..] line
       rowCells = map fst results
       rowTargets = concatMap snd results
   in (rowCells, rowTargets)

  parseCell r c char
   | char == '*' = (Air, [(r, c)])
   | otherwise = (charToCell char, [])

simulateSteps :: GameState -> [Direction] -> IO ()
simulateSteps initialState directions = do
 putStrLn "Initial state:"
 putStrLn $ showBoard (gsBoard initialState) (gsTarget initialState)
 go initialState directions (1 :: Int)
 where
  go _ [] _ = putStrLn "Complete!"
  go currentState (dir:rest) stepNum = do
   putStrLn $ "Step " ++ show stepNum ++ ": Apply gravity " ++ show dir
   case stepGame dir currentState of
    Left BatHitTarget -> do
     putStrLn $ showBoard (gsBoard currentState) (gsTarget currentState)
     putStrLn "Lost!"
    Left AllGemsCollected -> do
     putStrLn $ showBoard (gsBoard currentState) (gsTarget currentState)
     putStrLn "Won!"
    Right newState -> do
     putStrLn $ showBoard (gsBoard newState) (gsTarget newState)
     go newState rest (stepNum + 1)