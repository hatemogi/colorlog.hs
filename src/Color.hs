module Color
    ( mainLoop
    ) where

import Data.Char

lineColoring :: String -> [String]
lineColoring line =
  [(fmap toUpper line)]

mainLoop :: IO ()
mainLoop = do
  line <- getLine
  putStrLn (concat (lineColoring line))
  mainLoop
