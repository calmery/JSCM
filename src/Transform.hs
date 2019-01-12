module Transform (dummyResult) where

import           Check  (checkIsPureFunction)
import           Dummy  (dummyData)
import           Parser (Expression (..))
import           RIO

dummyResult :: Either String Bool
dummyResult = checkIsPureFunction functionDeclarations "getRandomPrimeNumber"
  where
    functionDeclarations = getFunctionDeclarations dummyData -- Temporal Dummy Data
    getFunctionDeclarations (JSProgram expressions) = map transformToDeclaration $ filter checkIsJSFunctionDeclaration expressions
    transformToDeclaration functionDeclaration@(JSFunctionDeclaration (JSIdentifier identifier) _ _) = (identifier, functionDeclaration, False)
    checkIsJSFunctionDeclaration (JSFunctionDeclaration _ _ _) = True
    checkIsJSFunctionDeclaration _                             = False
