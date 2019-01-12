module Check (checkIsPureFunction, JSFunctionDeclarations) where

import           NativeFunctionNames (nativeFunctionNames)
import           Parser              (Expression (..))
import           RIO

-- Types

type Declaration = (String, Expression, Bool)
type JSDeclarations = [Declaration]
type JSLocalDeclarations = [Declaration]
type JSUpdatedLocalDeclarations = [Declaration]
type JSFunctionDeclarations = [Declaration]
type JSFunctionIdentifier = String
type JSFunctionExpression = Expression

nativeFunctionDeclarations :: JSFunctionDeclarations
nativeFunctionDeclarations =
  map (\identifier -> (identifier, JSEmpty, True)) nativeFunctionNames

-- Get Function Declarations

checkIsPureFunction :: JSFunctionDeclarations -> JSFunctionIdentifier -> Either String Bool
checkIsPureFunction functionDeclarations identifier = case getJSFunctionExpression functionDeclarations identifier of
  Just expression -> Right $ isPureFunction (functionDeclarations ++ nativeFunctionDeclarations) expression -- isPureFunction' functionDeclarations functionDeclarations expression
  Nothing         -> Left $ identifier ++ " not found"
  where
    getJSFunctionExpression [] _ = Nothing
    getJSFunctionExpression ((label, expression, _):functionDeclarations) identifier =
      if label == identifier then
        Just expression
      else
        getJSFunctionExpression functionDeclarations identifier

-- Check

isPureFunction :: JSFunctionDeclarations -> JSFunctionExpression -> Bool
isPureFunction functionDeclarations expression = fst $ isPureFunction' functionDeclarations functionDeclarations expression

isPureFunction' :: JSFunctionDeclarations -> JSLocalDeclarations -> Expression -> (Bool, JSUpdatedLocalDeclarations)
isPureFunction' functionDeclarations localDeclarations (JSBlockStatement expressions) = checkExpressions functionDeclarations localDeclarations expressions
isPureFunction' _ localDeclarations (JSNumberLiteral _) = (True, localDeclarations)
isPureFunction' functionDeclarations localDeclarations (JSBinaryExpression "+" leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSBinaryExpression "-" leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSBinaryExpression "*" leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSBinaryExpression "/" leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSBinaryExpression "%" leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSBinaryExpression "===" leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSBinaryExpression "!==" leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSBinaryExpression ">" leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSBinaryExpression "<" leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSBinaryExpression "<=" leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' _ localDeclarations (JSBooleanLiteral _) = (True, localDeclarations)
isPureFunction' functionDeclarations localDeclarations (JSAssignmentExpression (JSVariableDeclaration (JSIdentifier identifier)) rightExpression) =
  let
    result = isPureFunction' functionDeclarations localDeclarations rightExpression
  in
    if fst result then
      (True, (identifier, rightExpression, True):localDeclarations)
    else
      result
isPureFunction' functionDeclarations localDeclarations (JSAssignmentExpression leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSIdentifier identifier) = findAndUpdate functionDeclarations localDeclarations identifier
isPureFunction' functionDeclarations localDeclarations (JSIfStatement condition expression Nothing) = checkExpressions functionDeclarations localDeclarations [condition, expression]
isPureFunction' functionDeclarations localDeclarations (JSIfStatement condition expression (Just elseExpression)) = checkExpressions functionDeclarations localDeclarations [condition, expression, elseExpression]
isPureFunction' functionDeclarations localDeclarations (JSForStatement (firstExpression, secondExpression, thirdExpression) body) = checkExpressions functionDeclarations localDeclarations [firstExpression, secondExpression, thirdExpression, body]
isPureFunction' _ localDeclarations JSEmpty = (True, localDeclarations)
isPureFunction' _ localDeclarations (JSStringLiteral _) = (True, localDeclarations)
isPureFunction' functionDeclarations localDeclarations functionDeclaration@(JSFunctionDeclaration (JSIdentifier identifier) arguments body) =
  let
    updatedFunctionDeclarations = updateDeclaration functionDeclarations identifier True
    updatedDeclarations = updatedFunctionDeclarations ++ map (\expression@(JSIdentifier identifier) -> (identifier, expression, True)) arguments
  in
    isPureFunction' updatedFunctionDeclarations updatedDeclarations body
isPureFunction' functionDeclarations localDeclarations (JSReturnStatement expression) = isPureFunction' functionDeclarations localDeclarations expression
isPureFunction' functionDeclarations localDeclarations (JSArrayExpression expressions) = checkExpressions functionDeclarations localDeclarations expressions
isPureFunction' functionDeclarations localDeclarations (JSBinaryExpression "&&" leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSBinaryExpression "||" leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSCallExpression arguments expression) = checkExpressions functionDeclarations localDeclarations (expression:arguments)
isPureFunction' functionDeclarations localDeclarations (JSMemberExpression leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSObjectMemberExpression _ rightExpression) = isPureFunction' functionDeclarations localDeclarations rightExpression
isPureFunction' functionDeclarations localDeclarations (JSBinaryExpression "&" leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSBinaryExpression "|" leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSBinaryExpression "<<" leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSBinaryExpression ">>" leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSPostfixUpdateExpression "++" expression) = isPureFunction' functionDeclarations localDeclarations expression
isPureFunction' _ _ expression = error $ show expression

-- Helpers

findAndUpdate :: JSFunctionDeclarations -> JSLocalDeclarations -> JSFunctionIdentifier -> (Bool, JSUpdatedLocalDeclarations)
findAndUpdate functionDeclarations localDeclarations = findAndUpdate' functionDeclarations localDeclarations localDeclarations
  where
    findAndUpdate' _ localDeclarationsOrigin [] _ = (False, localDeclarationsOrigin)
    findAndUpdate' functionDeclarations localDeclarationsOrigin ((label, expression, alreadyChecked):localDeclarations) identifier =
      if label == identifier then
        if alreadyChecked then
          (True, localDeclarationsOrigin)
        else
          let
            result = isPureFunction functionDeclarations expression
          in
            (result, updateDeclaration localDeclarationsOrigin label result)
      else
        findAndUpdate' functionDeclarations localDeclarationsOrigin localDeclarations identifier

-- Update Declaration

updateDeclaration :: JSDeclarations -> String -> Bool -> JSDeclarations
updateDeclaration [] _ _ = []
updateDeclaration (declaration@(label, expression, _):declarations) identifier checkStatus =
  if label == identifier then
    (label, expression, checkStatus):declarations
  else
    declaration:updateDeclaration declarations identifier checkStatus

-- Check Function Arguments is Pure

checkExpressions :: JSFunctionDeclarations -> JSLocalDeclarations -> [Expression] -> (Bool, JSUpdatedLocalDeclarations)
checkExpressions functionDeclarations localDeclarations [] = (True, localDeclarations)
checkExpressions functionDeclarations localDeclarations (argument:arguments) =
  let
    result = isPureFunction' functionDeclarations localDeclarations argument
  in
    if fst result then
      checkExpressions functionDeclarations (snd result) arguments
    else
      result
