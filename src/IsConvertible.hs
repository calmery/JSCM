module IsConvertible (Declaration, isConvertible) where

import           Parser (Expression (..))
import           RIO

type Declaration = (String, Expression, Bool) -- (Identifier, Expression, Searched)
type FunctionDeclaration = Declaration
type FunctionIdentifier = String
type FunctionExpression = Expression

-- Build-in objects and functions

nativeFunctionDeclarations :: [FunctionDeclaration]
nativeFunctionDeclarations =
  map
    (\identifier -> (identifier, JSEmpty, True))
    [ "BigInt"
    , "Math"
    , "parseInt"
    , "console"
    ]

-- Main

isConvertible :: [FunctionDeclaration] -> FunctionIdentifier -> Either String Bool
isConvertible functionDeclarations identifier = case getFunctionExpression functionDeclarations identifier of
  Just expression -> Right $ isReferentiallyTransparentFunction (functionDeclarations ++ nativeFunctionDeclarations) expression
  Nothing         -> Left $ identifier ++ " not found"
  where
    getFunctionExpression [] _ = Nothing
    getFunctionExpression ((label, expression, _):functionDeclarations) identifier =
      if label == identifier then
        Just expression
      else
        getFunctionExpression functionDeclarations identifier

type LocalDeclaration = Declaration
type UpdatedLocalDeclaration = Declaration

isReferentiallyTransparentFunction :: [FunctionDeclaration] -> FunctionExpression -> Bool
isReferentiallyTransparentFunction functionDeclarations expression = fst $ isReferentiallyTransparentFunction' functionDeclarations functionDeclarations expression

isReferentiallyTransparentFunction' :: [FunctionDeclaration] -> [LocalDeclaration] -> Expression -> (Bool, [UpdatedLocalDeclaration])
isReferentiallyTransparentFunction' functionDeclarations localDeclarations (JSIdentifier identifier) = findAndUpdate functionDeclarations localDeclarations identifier
isReferentiallyTransparentFunction' _ localDeclarations (JSBooleanLiteral _) = (True, localDeclarations)
isReferentiallyTransparentFunction' _ localDeclarations (JSNumberLiteral _) = (True, localDeclarations)
isReferentiallyTransparentFunction' _ localDeclarations (JSStringLiteral _) = (True, localDeclarations)
isReferentiallyTransparentFunction' functionDeclarations localDeclarations (JSArrayExpression expressions) = checkExpressions functionDeclarations localDeclarations expressions
isReferentiallyTransparentFunction' functionDeclarations localDeclarations (JSAssignmentExpression (JSVariableDeclaration (JSIdentifier identifier)) rightExpression) =
  let
    result = isReferentiallyTransparentFunction' functionDeclarations localDeclarations rightExpression
  in
    if fst result then
      (True, (identifier, rightExpression, True):localDeclarations)
    else
      result
isReferentiallyTransparentFunction' functionDeclarations localDeclarations (JSAssignmentExpression leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isReferentiallyTransparentFunction' functionDeclarations localDeclarations (JSBinaryExpression _ leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isReferentiallyTransparentFunction' functionDeclarations localDeclarations (JSCallExpression arguments expression) = checkExpressions functionDeclarations localDeclarations (expression:arguments)
isReferentiallyTransparentFunction' functionDeclarations localDeclarations (JSMemberExpression leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isReferentiallyTransparentFunction' functionDeclarations localDeclarations (JSObjectMemberExpression _ rightExpression) = isReferentiallyTransparentFunction' functionDeclarations localDeclarations rightExpression
isReferentiallyTransparentFunction' functionDeclarations localDeclarations (JSPostfixUpdateExpression _ expression) = isReferentiallyTransparentFunction' functionDeclarations localDeclarations expression
isReferentiallyTransparentFunction' functionDeclarations localDeclarations (JSBlockStatement expressions) = checkExpressions functionDeclarations localDeclarations expressions
isReferentiallyTransparentFunction' functionDeclarations localDeclarations (JSForStatement (firstExpression, secondExpression, thirdExpression) body) = checkExpressions functionDeclarations localDeclarations [firstExpression, secondExpression, thirdExpression, body]
isReferentiallyTransparentFunction' functionDeclarations localDeclarations (JSIfStatement condition expression Nothing) = checkExpressions functionDeclarations localDeclarations [condition, expression]
isReferentiallyTransparentFunction' functionDeclarations localDeclarations (JSIfStatement condition expression (Just elseExpression)) = checkExpressions functionDeclarations localDeclarations [condition, expression, elseExpression]
isReferentiallyTransparentFunction' functionDeclarations localDeclarations (JSReturnStatement expression) = isReferentiallyTransparentFunction' functionDeclarations localDeclarations expression
isReferentiallyTransparentFunction' functionDeclarations localDeclarations functionDeclaration@(JSFunctionDeclaration (JSIdentifier identifier) arguments body) =
  let
    updatedFunctionDeclarations = updateDeclaration functionDeclarations identifier True
    updatedDeclarations = updatedFunctionDeclarations ++ map (\expression@(JSIdentifier identifier) -> (identifier, expression, True)) arguments
  in
    isReferentiallyTransparentFunction' updatedFunctionDeclarations updatedDeclarations body
isReferentiallyTransparentFunction' _ localDeclarations JSEmpty = (True, localDeclarations)
isReferentiallyTransparentFunction' _ _ expression = error $ show expression

-- Helpers

findAndUpdate :: [FunctionDeclaration] -> [LocalDeclaration] -> FunctionIdentifier -> (Bool, [UpdatedLocalDeclaration])
findAndUpdate functionDeclarations localDeclarations = findAndUpdate' functionDeclarations localDeclarations localDeclarations
  where
    findAndUpdate' _ localDeclarationsOrigin [] _ = (False, localDeclarationsOrigin)
    findAndUpdate' functionDeclarations localDeclarationsOrigin ((label, expression, alreadyChecked):localDeclarations) identifier =
      if label == identifier then
        if alreadyChecked then
          (True, localDeclarationsOrigin)
        else
          let
            result = isReferentiallyTransparentFunction functionDeclarations expression
          in
            (result, updateDeclaration localDeclarationsOrigin label result)
      else
        findAndUpdate' functionDeclarations localDeclarationsOrigin localDeclarations identifier

updateDeclaration :: [Declaration] -> String -> Bool -> [Declaration]
updateDeclaration [] _ _ = []
updateDeclaration (declaration@(label, expression, _):declarations) identifier checkStatus =
  if label == identifier then
    (label, expression, checkStatus):declarations
  else
    declaration:updateDeclaration declarations identifier checkStatus

checkExpressions :: [FunctionDeclaration] -> [LocalDeclaration] -> [Expression] -> (Bool, [UpdatedLocalDeclaration])
checkExpressions functionDeclarations localDeclarations [] = (True, localDeclarations)
checkExpressions functionDeclarations localDeclarations (argument:arguments) =
  let
    result = isReferentiallyTransparentFunction' functionDeclarations localDeclarations argument
  in
    if fst result then
      checkExpressions functionDeclarations (snd result) arguments
    else
      result
