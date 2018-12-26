module Lib
    ( someFunc
    ) where

import           RIO
import           System.IO

someFunc :: IO ()
someFunc = putStrLn "someFunc"
