module Transform (transform) where

import           Check    (JSFunctionDeclarations, checkIsPureFunction)
import           Parser   (Expression (..))
import           RIO
import           ToString (toString)

type JSProgramExpression = Expression

transform :: JSProgramExpression -> JSProgramExpression
transform (JSProgram expressions) = JSProgram (transform' callExpressionIdentifiers functionDeclarations expressions)
  where
    callExpressionIdentifiers = getCallExpressionIdentifiers expressions
    functionDeclarations = getFunctionDeclarations expressions
    getCallExpressionIdentifiers []              = []
    getCallExpressionIdentifiers (callExpression@(JSCall _ (JSIdentifier identifier)):expressions) = identifier:getCallExpressionIdentifiers expressions
    getCallExpressionIdentifiers (_:expressions) = getCallExpressionIdentifiers expressions
    getFunctionDeclarations expressions = map transformToDeclaration $ filter checkIsJSFunctionDeclaration expressions
    transformToDeclaration functionDeclaration@(JSFunctionDeclaration (JSIdentifier identifier) _ _) = (identifier, functionDeclaration, False)
    checkIsJSFunctionDeclaration (JSFunctionDeclaration _ _ _) = True
    checkIsJSFunctionDeclaration _                             = False

transform' _ _ []                       = []
transform' callExpressionIdentifiers functionDeclarations (expression@(JSFunctionDeclaration (JSIdentifier identifier) arguments _):expressions) =
  if elem identifier callExpressionIdentifiers then
    (case checkIsPureFunction functionDeclarations identifier of
      Right isPure ->
        if isPure then
          JSFunctionDeclaration
            (JSIdentifier identifier)
            arguments
            (JSBlock
              [JSNativeCode $
                "var _w=new Worker(URL.createObjectURL(new Blob([`" ++
                (unwords $ map (\(_, expression, _) -> toString expression) functionDeclarations) ++
                "onmessage=function(e){" ++ identifier ++ ".apply(this,e.data)};" ++
                "`])));" ++
                "_w.postMessage([" ++ (unwords $ map (\(JSIdentifier identifier) -> identifier ++ ",") arguments) ++ "]);"
              ]
            )
        else
          expression

      Left _ ->
        expression
    ):transform' callExpressionIdentifiers functionDeclarations expressions
  else
    expression:transform' callExpressionIdentifiers functionDeclarations expressions
transform' callExpressionIdentifiers functionDeclarations (expression:expressions) = expression:transform' callExpressionIdentifiers functionDeclarations expressions
