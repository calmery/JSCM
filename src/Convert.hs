module Convert (convert) where

import           AST           (Expression (..))
import           IsConvertible (Declaration, isConvertible)
import           RIO
import           ToString      (toString)

convert :: Expression -> Expression
convert (JSProgram expressions) = JSProgram (convert' callExpressionIdentifiers functionDeclarations expressions)
  where
    callExpressionIdentifiers = getCallExpressionIdentifiers expressions
    functionDeclarations = convertFunctionDeclarationsToDeclarations $ getFunctionDeclarations expressions

convert' :: [String] -> [Declaration] -> [Expression] -> [Expression]
convert' _ _ []                       = []
convert' callExpressionIdentifiers functionDeclarations (expression@(JSFunctionDeclaration (JSIdentifier identifier) arguments _):expressions) =
  (if elem identifier callExpressionIdentifiers then
    case isConvertible functionDeclarations identifier of
      Right isPure ->
        if isPure then
          JSFunctionDeclaration
            (JSIdentifier identifier)
            arguments
            (JSBlockStatement
              [JSInternalCode $
                "var _w=new Worker(URL.createObjectURL(new Blob([`" ++
                (unwords $ map (\(_, expression, _) -> toString expression) functionDeclarations) ++
                "onmessage=function(e){postMessage(" ++ identifier ++ ".apply(this,e.data));};" ++
                "`])));" ++
                "_w.postMessage([" ++ (unwords $ map (\(JSIdentifier identifier) -> identifier ++ ",") arguments) ++ "]);" ++
                "return new Promise(r=>{_w.onmessage=function(e){r(e.data);_w.terminate();};});"
              ]
            )
        else
          expression

      Left _ ->
        expression
  else
    expression
  ):convert' callExpressionIdentifiers functionDeclarations expressions
convert' callExpressionIdentifiers functionDeclarations (assignmentExpression@(JSAssignmentExpression variableIdentifier expression@(JSCallExpression arguments (JSIdentifier identifier))):expressions) =
  (if elem identifier callExpressionIdentifiers then
    let
      innerExpressions = convert' callExpressionIdentifiers functionDeclarations expressions
    in
      [JSCallExpression [
        JSFunctionDeclaration (JSIdentifier "") [
          case variableIdentifier of
            JSVariableDeclaration i ->
              i

            _ ->
              variableIdentifier
        ] (JSBlockStatement innerExpressions)
      ] (JSObjectMemberExpression (JSIdentifier "then") expression)]
  else
    expression:(convert' callExpressionIdentifiers functionDeclarations expressions)
  )
convert' callExpressionIdentifiers functionDeclarations (expression:expressions) = expression:convert' callExpressionIdentifiers functionDeclarations expressions

getCallExpressionIdentifiers :: [Expression] -> [String]
getCallExpressionIdentifiers []              = []
getCallExpressionIdentifiers (callExpression@(JSCallExpression _ (JSIdentifier identifier)):expressions) = identifier:getCallExpressionIdentifiers expressions
getCallExpressionIdentifiers (assignmentExpression@(JSAssignmentExpression _ (JSCallExpression _ (JSIdentifier identifier))):expressions) = identifier:getCallExpressionIdentifiers expressions
getCallExpressionIdentifiers (_:expressions) = getCallExpressionIdentifiers expressions

getFunctionDeclarations :: [Expression] -> [Expression]
getFunctionDeclarations expressions = filter isJSFunctionDeclaration expressions
  where
    isJSFunctionDeclaration (JSFunctionDeclaration _ _ _) = True
    isJSFunctionDeclaration _                             = False

convertFunctionDeclarationsToDeclarations :: [Expression] -> [Declaration]
convertFunctionDeclarationsToDeclarations = map (\functionDeclaration@(JSFunctionDeclaration (JSIdentifier identifier) _ _) -> (identifier, functionDeclaration, False))
