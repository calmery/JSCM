module Main where

import           Parser             (parse)
import           RIO                hiding (hClose)
import           RIO.List.Partial   (head)
import           System.Environment (getArgs)
import           System.IO          (IOMode (ReadMode), hClose, hGetContents,
                                     openFile, putStrLn)
import           ToString           (toString)
import           Transform          (transform)

main :: IO ()
main = do
  args <- getArgs
  if 1 <= length args then do
    let fileName = head args
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    putStrLn $ case parse fileName contents of
      Left e       -> show e
      Right result -> toString $ transform result
    hClose handle
  else
    putStrLn "Argument not specified"
