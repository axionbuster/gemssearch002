{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main (main) where

import           Control.Monad
import           Control.Monad.ST.Strict
import           TotM

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
 let gameState = GameState board target

 putStrLn $ "Test case " ++ show testNum ++ ":"

 case solve gameState of
  Nothing -> putStrLn "no"
  Just directions -> do
   putStrLn "yes"
   simulateSteps gameState directions

parseGrid :: [String] -> ([[Cell]], (Int, Int))
parseGrid gridLines =
 let -- Convert each line to cells and collect target positions
     results = zipWith parseRow [0..] gridLines
     cellRows = map fst results
     allTargets = concatMap snd results
     target = head allTargets  -- assume exactly one target
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
   let (newBoard, outcome) = runST $ applyGravity dir (gsTarget currentState) (gsBoard currentState)
   let newState = GameState newBoard (gsTarget currentState)
   putStrLn $ showBoard newBoard (gsTarget currentState)
   case outcome of
    Won     -> putStrLn "Won!"
    Lost    -> putStrLn "Lost!"
    Running -> go newState rest (stepNum + 1)
