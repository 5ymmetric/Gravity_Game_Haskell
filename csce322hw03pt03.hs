import Prelude
import System.Environment ( getArgs )
import Data.List
import Data.Char
import Data.Maybe
import Data.Function ( on )
import Helpers
  
-- The main method that will be used for testing / command line access
main = do
 args <- getArgs
 filename <- readFile (head args)
 (maze,moves) <- readGravityMazeFile filename
 print "Result"
 printMaze (manyPlayersManyRotations maze moves)

-- YOUR CODE SHOULD COME AFTER THIS POINT
manyPlayersManyRotations :: [[Char]] -> [[Char]] -> [[Char]]
manyPlayersManyRotations maze [] = maze
manyPlayersManyRotations maze (move:moves)
    | isSolved maze = maze
    | otherwise = manyPlayersManyRotations (onePlayerOneRotation maze move) moves

onePlayerOneRotation :: [[Char]] -> [Char] -> [[Char]]
onePlayerOneRotation maze move = if isSolved maze
    then maze
    else rotate maze move

clockwise :: [[Char]] -> [[Char]]
clockwise maze = transpose (reverse maze)

counterClockwise :: [[Char]] -> [[Char]]
counterClockwise maze = reverse (transpose maze)

rotate :: [[Char]] -> [Char] -> [[Char]]
rotate maze move
    | (move == "c") = applyGravityForAllPlayers (clockwise maze) (getSortedList (clockwise maze)) (findPlayerOrder (clockwise maze))
    | (move == "cc") = applyGravityForAllPlayers (counterClockwise maze) (getSortedList (counterClockwise maze)) (findPlayerOrder (counterClockwise maze))
    | (move == "180") = applyGravityForAllPlayers (clockwise (clockwise maze)) (getSortedList (clockwise (clockwise maze))) (findPlayerOrder (clockwise (clockwise maze)))

playerFinder :: [[Char]] -> Char -> ([Int], [Int])
playerFinder maze n = (catMaybes (map (elemIndex n) (transpose maze)), (catMaybes (map (elemIndex n) maze)))

playerList :: [[Char]] -> [([Int], [Int])]
playerList maze = (playerFinder maze '1') : (playerFinder maze '2') : (playerFinder maze '3') : (playerFinder maze '4') : []

clearingPlayerList :: [[Char]] -> [([Int], [Int])]
clearingPlayerList maze = delete ([], []) (delete ([], []) (delete ([], []) (playerList maze)))

nonEmptyPlayerFinder :: [[Char]] -> Char -> (Int, Int)
nonEmptyPlayerFinder maze n = (head (catMaybes (map (elemIndex n) (transpose maze))), head (catMaybes (map (elemIndex n) maze)))

finalPlayerFinder :: [[Char]] -> Int -> [(Int, Int)]
finalPlayerFinder maze n = case n of
    1 -> (nonEmptyPlayerFinder maze '1') :[]
    2 -> (nonEmptyPlayerFinder maze '1') : (nonEmptyPlayerFinder maze '2') : []
    3 -> (nonEmptyPlayerFinder maze '1') : (nonEmptyPlayerFinder maze '2') : (nonEmptyPlayerFinder maze '3') : []
    4 -> (nonEmptyPlayerFinder maze '1') : (nonEmptyPlayerFinder maze '2') : (nonEmptyPlayerFinder maze '3') : (nonEmptyPlayerFinder maze '4') : []

finalPlayerList :: [[Char]] -> [([Int], [Int])] -> [(Int, Int)]
finalPlayerList maze mazeList = finalPlayerFinder maze (length mazeList)

getSortedList :: [[Char]] -> [(Int, Int)]
getSortedList maze = reverse (sortBy (compare `on` fst) (finalPlayerList maze (clearingPlayerList maze)))

getPlayerColumn :: [[Char]] -> Int -> [Char]
getPlayerColumn maze column = map (!! column) maze

isSolved :: [[Char]] -> Bool
isSolved [] = True
isSolved (x:xs)
    | 'g' `elem` x = False
    | otherwise = isSolved xs

subListMaker :: [Char] -> Int -> [Char]
subListMaker playerColumn playerRow = drop (playerRow + 1) playerColumn

stopFinder :: [Char] -> Char
stopFinder [] = ' '
stopFinder playerColumnBelow = if (head playerColumnBelow == 'g')
    then 'g'
    else if (head playerColumnBelow == 'x')
        then 'x'
        else if (head playerColumnBelow == '1')
            then '1'
            else if (head playerColumnBelow == '2')
                then '2'
                else if (head playerColumnBelow == '3')
                    then '3'
                    else if (head playerColumnBelow == '4')
                        then '4'
                        else stopFinder (tail playerColumnBelow)

stopGIndexFinder :: [Char] ->  Int
stopGIndexFinder playerColumnBelow = fromJust (elemIndex (stopFinder playerColumnBelow) playerColumnBelow)

stopXIndexFinder :: [Char] ->  Int
stopXIndexFinder playerColumnBelow = fromJust (elemIndex (stopFinder playerColumnBelow) playerColumnBelow) - 1

stopIndexFinder :: [Char] -> (Int, Int) -> (Int, Int)
stopIndexFinder playerColumn playerPosition = if (stopFinder (subListMaker playerColumn (fst playerPosition)) == 'x')
    then ((fst playerPosition) + (stopXIndexFinder (subListMaker playerColumn (fst playerPosition))) + 1, snd playerPosition)
    else if (stopFinder (subListMaker playerColumn (fst playerPosition)) == 'g')
        then ((fst playerPosition) + (stopGIndexFinder (subListMaker playerColumn (fst playerPosition))) + 1, snd playerPosition)
    else ((fst playerPosition) + (stopXIndexFinder (subListMaker playerColumn (fst playerPosition))) + 1, snd playerPosition)

getStopIndex :: [[Char]] -> (Int, Int) -> (Int, Int)
getStopIndex maze playerPosition = stopIndexFinder (getPlayerColumn maze (snd (head (getSortedList maze)))) playerPosition

updateMatrix :: [[Char]] -> Char -> (Int, Int) -> [[Char]]
updateMatrix maze player (row, column) =
  take row maze ++ [take column (maze !! row) ++ [player] ++ drop (column + 1) (maze !! row)] ++ drop (row + 1) maze

findPlayerOrder :: [[Char]] -> [Char]
findPlayerOrder maze = [x | x <- (concat (clockwise (clockwise maze))), x `elem` ['1', '2', '3', '4']]

applyGravityForAllPlayers :: [[Char]] -> [(Int, Int)] -> [Char] -> [[Char]]
applyGravityForAllPlayers maze positions [] = maze
applyGravityForAllPlayers maze [] players = maze
applyGravityForAllPlayers maze (playerPosition:playerPositions) (player:players) = applyGravityForAllPlayers (applyGravityForOnePlayer maze playerPosition player) playerPositions players

applyGravityForOnePlayer :: [[Char]] -> (Int, Int) -> Char -> [[Char]]
applyGravityForOnePlayer maze playerPosition player = updateMatrix (updateMatrix maze '-' playerPosition) player (getStopIndex maze playerPosition)