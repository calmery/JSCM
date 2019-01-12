module Check (result) where

import           Dummy  (dummyData)
import           Parser (Expression (..))
import           RIO

-- Types

type Declaration = (String, Expression, Bool)
type JSDeclarations = [Declaration]
type JSLocalDeclarations = [Declaration]
type JSUpdatedLocalDeclarations = [Declaration]
type JSFunctionDeclarations = [Declaration]
type JSFunctionIdentifier = String
type JSFunctionExpression = Expression

-- Result

result :: Either String Bool
result = checkIsPureFunction functionDeclarations "getRandomPrimeNumber"
  where
    functionDeclarations = getFunctionDeclarations dummyData -- Temporal Dummy Data
    getFunctionDeclarations (JSProgram expressions) = map transformToDeclaration $ filter checkIsJSFunctionDeclaration expressions
    transformToDeclaration functionDeclaration@(JSFunctionDeclaration (JSIdentifier identifier) _ _) = (identifier, functionDeclaration, False)
    checkIsJSFunctionDeclaration (JSFunctionDeclaration _ _ _) = True
    checkIsJSFunctionDeclaration _                             = False

-- Get Function Declarations

checkIsPureFunction :: JSFunctionDeclarations -> JSFunctionIdentifier -> Either String Bool
checkIsPureFunction functionDeclarations identifier = case getJSFunctionExpression functionDeclarations identifier of
  Just expression -> Right $ isPureFunction functionDeclarations expression -- isPureFunction' functionDeclarations functionDeclarations expression
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
isPureFunction' functionDeclarations localDeclarations (JSBlock expressions) = checkExpressions functionDeclarations localDeclarations expressions
isPureFunction' _ localDeclarations (JSNumber _) = (True, localDeclarations)
isPureFunction' functionDeclarations localDeclarations (JSPlus leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSMinus leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSTimes leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSDivide leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSModulo leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSStrictEqual leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSStrictNotEqual leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSGreater leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSLess leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSLessOrEqual leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' _ localDeclarations (JSBoolean _) = (True, localDeclarations)
isPureFunction' functionDeclarations localDeclarations (JSAssignment (JSVariableDeclaration (JSIdentifier identifier)) rightExpression) =
  let
    result = isPureFunction' functionDeclarations localDeclarations rightExpression
  in
    if fst result then
      (True, (identifier, rightExpression, True):localDeclarations)
    else
      result
isPureFunction' functionDeclarations localDeclarations (JSAssignment leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSIdentifier identifier) = findAndUpdate functionDeclarations localDeclarations identifier
isPureFunction' functionDeclarations localDeclarations (JSIf condition expression Nothing) = checkExpressions functionDeclarations localDeclarations [condition, expression]
isPureFunction' functionDeclarations localDeclarations (JSIf condition expression (Just elseExpression)) = checkExpressions functionDeclarations localDeclarations [condition, expression, elseExpression]
isPureFunction' functionDeclarations localDeclarations (JSFor (firstExpression, secondExpression, thirdExpression) body) = checkExpressions functionDeclarations localDeclarations [firstExpression, secondExpression, thirdExpression, body]
isPureFunction' _ localDeclarations JSEmpty = (True, localDeclarations)
isPureFunction' _ localDeclarations (JSString _) = (True, localDeclarations)
isPureFunction' functionDeclarations localDeclarations functionDeclaration@(JSFunctionDeclaration (JSIdentifier identifier) arguments body) =
  let
    updatedFunctionDeclarations = updateDeclaration functionDeclarations identifier True
    updatedDeclarations = updatedFunctionDeclarations ++ (map (\expression@(JSIdentifier identifier) -> (identifier, expression, True)) arguments)
  in
    isPureFunction' updatedFunctionDeclarations updatedDeclarations body
isPureFunction' functionDeclarations localDeclarations (JSReturn expression) = isPureFunction' functionDeclarations localDeclarations expression
isPureFunction' functionDeclarations localDeclarations (JSArray expressions) = checkExpressions functionDeclarations localDeclarations expressions
isPureFunction' functionDeclarations localDeclarations (JSAndLogical leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSOrLogical leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSCall arguments expression) = checkExpressions functionDeclarations localDeclarations (expression:arguments)
isPureFunction' functionDeclarations localDeclarations (JSMember leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSObjectMember _ rightExpression) = isPureFunction' functionDeclarations localDeclarations rightExpression
isPureFunction' functionDeclarations localDeclarations (JSAnd leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSOr leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSLeftShift leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSRightShift leftExpression rightExpression) = checkExpressions functionDeclarations localDeclarations [leftExpression, rightExpression]
isPureFunction' functionDeclarations localDeclarations (JSPostfixPlusUpdate expression) = isPureFunction' functionDeclarations localDeclarations expression
isPureFunction' _ _ expression = error $ show expression

-- Helpers

findAndUpdate :: JSFunctionDeclarations -> JSLocalDeclarations -> JSFunctionIdentifier -> (Bool, JSUpdatedLocalDeclarations)
findAndUpdate functionDeclarations localDeclarations identifier = findAndUpdate' functionDeclarations localDeclarations localDeclarations identifier
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
    declaration:(updateDeclaration declarations identifier checkStatus)

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
