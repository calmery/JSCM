module NativeFunctionNames (nativeFunctionNames) where

import           RIO

-- Built-in objects and functions

nativeFunctionNames :: [String]
nativeFunctionNames =
  [ "BigInt"
  , "Math"
  , "parseInt"
  , "console"
  ]
