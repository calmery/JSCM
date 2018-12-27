module Main where

import           Parser    (parse)
import           RIO
import           System.IO

main :: IO ()
main = loop
  where
    loop = do
      input <- getLine
      putStrLn $ parse input
      loop
