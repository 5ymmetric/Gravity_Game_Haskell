module Helpers
( readGravityMazeFile
, printMaze
) where

import Prelude
import Data.Char
import Data.List

readGravityMazeFile :: String -> IO ([[Char]],[[Char]])
readGravityMazeFile = readIO

printMaze :: [[Char]] -> IO ()
printMaze [] = do
 print ""
printMaze (ro:ros) = do
 print ro
 printMaze ros