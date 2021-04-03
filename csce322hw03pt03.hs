import Prelude
import System.Environment ( getArgs )
import Data.List
import Data.Char
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
manyPlayersManyRotations maze moves = maze