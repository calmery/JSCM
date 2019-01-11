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

-- JSFunctionDeclaration

isPureFunction' functionDeclarations localDeclarations functionDeclaration@(JSFunctionDeclaration (JSIdentifier identifier) arguments body) =
  let
    updatedFunctionDeclarations = updateDeclaration functionDeclarations identifier True
    updatedDeclarations = updatedFunctionDeclarations ++ (map (\expression@(JSIdentifier identifier) -> (identifier, expression, True)) arguments)
  in
    isPureFunction' updatedFunctionDeclarations updatedDeclarations body

-- JSBlock

isPureFunction' _ localDeclarations (JSBlock []) = (True, localDeclarations)
isPureFunction' functionDeclarations localDeclarations (JSBlock ((JSAssignment (JSVariableDeclaration (JSIdentifier identifier)) expression):expressions)) =
  let
    result = isPureFunction' functionDeclarations localDeclarations expression
  in
    if fst result then
      isPureFunction' functionDeclarations ((identifier, expression, True):(snd result)) (JSBlock expressions)
    else
      result
isPureFunction' functionDeclarations localDeclarations (JSBlock (expression:expressions)) =
  let
    result = isPureFunction' functionDeclarations localDeclarations expression
  in
    if fst result then
      isPureFunction' functionDeclarations (snd result) (JSBlock expressions)
    else
      result

-- JSIf

isPureFunction' functionDeclarations localDeclarations (JSIf condition expression Nothing) =
  let
    result = isPureFunction' functionDeclarations localDeclarations condition
  in
    if fst result then
      isPureFunction' functionDeclarations (snd result) expression
    else
      result
isPureFunction' functionDeclarations localDeclarations (JSIf condition expression (Just elseExpression)) =
  let
    result = isPureFunction' functionDeclarations localDeclarations condition
  in
    if fst result then
      let
        expressionResult = isPureFunction' functionDeclarations (snd result) expression
      in
        if fst expressionResult then
          isPureFunction' functionDeclarations (snd expressionResult) elseExpression
        else
          expressionResult
    else
      result

-- JSReturn

isPureFunction' functionDeclarations localDeclarations (JSReturn expression) = isPureFunction' functionDeclarations localDeclarations expression

-- JSCall

isPureFunction' functionDeclarations localDeclarations (JSCall arguments expression) =
  let
    result = isPureFunction' functionDeclarations localDeclarations expression
  in
    if fst result then
      checkExpressions functionDeclarations (snd result) arguments
    else
      result

-- JSIdentifier

isPureFunction' functionDeclarations localDeclarations (JSIdentifier identifier) = findAndUpdate functionDeclarations localDeclarations identifier

-- JSNumber

isPureFunction' _ localDeclarations (JSNumber _) = (True, localDeclarations)

-- JSPlus

isPureFunction' functionDeclarations localDeclarations (JSPlus leftExpression rightExpression) =
  let
    leftResult = isPureFunction' functionDeclarations localDeclarations leftExpression
  in
    if fst leftResult then
      isPureFunction' functionDeclarations (snd leftResult) rightExpression
    else
      leftResult

-- JSMinus

isPureFunction' functionDeclarations localDeclarations (JSMinus leftExpression rightExpression) =
  let
    leftResult = isPureFunction' functionDeclarations localDeclarations leftExpression
  in
    if fst leftResult then
      isPureFunction' functionDeclarations (snd leftResult) rightExpression
    else
      leftResult

-- JSTimes

isPureFunction' functionDeclarations localDeclarations (JSTimes leftExpression rightExpression) =
  let
    leftResult = isPureFunction' functionDeclarations localDeclarations leftExpression
  in
    if fst leftResult then
      isPureFunction' functionDeclarations (snd leftResult) rightExpression
    else
      leftResult

-- JSOrLogical

isPureFunction' functionDeclarations localDeclarations (JSOrLogical leftExpression rightExpression) =
  let
    leftResult = isPureFunction' functionDeclarations localDeclarations leftExpression
  in
    if fst leftResult then
      isPureFunction' functionDeclarations (snd leftResult) rightExpression
    else
      leftResult

-- JSStrictEqual

isPureFunction' functionDeclarations localDeclarations (JSStrictEqual leftExpression rightExpression) =
  let
    leftResult = isPureFunction' functionDeclarations localDeclarations leftExpression
  in
    if fst leftResult then
      isPureFunction' functionDeclarations (snd leftResult) rightExpression
    else
      leftResult

-- JSLessOrEqual

isPureFunction' functionDeclarations localDeclarations (JSLessOrEqual leftExpression rightExpression) =
  let
    leftResult = isPureFunction' functionDeclarations localDeclarations leftExpression
  in
    if fst leftResult then
      isPureFunction' functionDeclarations (snd leftResult) rightExpression
    else
      leftResult

-- JSModulo

isPureFunction' functionDeclarations localDeclarations (JSModulo leftExpression rightExpression) =
  let
    leftResult = isPureFunction' functionDeclarations localDeclarations leftExpression
  in
    if fst leftResult then
      isPureFunction' functionDeclarations (snd leftResult) rightExpression
    else
      leftResult

-- JSDivide

isPureFunction' functionDeclarations localDeclarations (JSDivide leftExpression rightExpression) =
  let
    leftResult = isPureFunction' functionDeclarations localDeclarations leftExpression
  in
    if fst leftResult then
      isPureFunction' functionDeclarations (snd leftResult) rightExpression
    else
      leftResult

-- JSAndLogical

isPureFunction' functionDeclarations localDeclarations (JSAndLogical leftExpression rightExpression) =
  let
    leftResult = isPureFunction' functionDeclarations localDeclarations leftExpression
  in
    if fst leftResult then
      isPureFunction' functionDeclarations (snd leftResult) rightExpression
    else
      leftResult

-- JSStrictNotEqual

isPureFunction' functionDeclarations localDeclarations (JSStrictNotEqual leftExpression rightExpression) =
  let
    leftResult = isPureFunction' functionDeclarations localDeclarations leftExpression
  in
    if fst leftResult then
      isPureFunction' functionDeclarations (snd leftResult) rightExpression
    else
      leftResult

-- JSGreater

isPureFunction' functionDeclarations localDeclarations (JSGreater leftExpression rightExpression) =
  let
    leftResult = isPureFunction' functionDeclarations localDeclarations leftExpression
  in
    if fst leftResult then
      isPureFunction' functionDeclarations (snd leftResult) rightExpression
    else
      leftResult

-- JSRightShift

isPureFunction' functionDeclarations localDeclarations (JSRightShift leftExpression rightExpression) =
  let
    leftResult = isPureFunction' functionDeclarations localDeclarations leftExpression
  in
    if fst leftResult then
      isPureFunction' functionDeclarations (snd leftResult) rightExpression
    else
      leftResult

-- JSMember

isPureFunction' functionDeclarations localDeclarations (JSMember leftExpression rightExpression) =
  let
    leftResult = isPureFunction' functionDeclarations localDeclarations leftExpression
  in
    if fst leftResult then
      isPureFunction' functionDeclarations (snd leftResult) rightExpression
    else
      leftResult

-- JSEmpty

isPureFunction' _ localDeclarations JSEmpty = (True, localDeclarations)

-- JSBoolean

isPureFunction' _ localDeclarations (JSBoolean _) = (True, localDeclarations)

-- JSAnd

isPureFunction' functionDeclarations localDeclarations (JSAnd leftExpression rightExpression) =
  let
    leftResult = isPureFunction' functionDeclarations localDeclarations leftExpression
  in
    if fst leftResult then
      isPureFunction' functionDeclarations (snd leftResult) rightExpression
    else
      leftResult

-- JSLeftShift

isPureFunction' functionDeclarations localDeclarations (JSLeftShift leftExpression rightExpression) =
  let
    leftResult = isPureFunction' functionDeclarations localDeclarations leftExpression
  in
    if fst leftResult then
      isPureFunction' functionDeclarations (snd leftResult) rightExpression
    else
      leftResult

-- JSArray

isPureFunction' functionDeclarations localDeclarations (JSArray expressions) =
  checkExpressions functionDeclarations localDeclarations expressions

-- JSFor

isPureFunction' functionDeclarations localDeclarations (JSFor (firstExpression, secondExpression, thirdExpression) body) =
  let
    firstResult = isPureFunction' functionDeclarations localDeclarations firstExpression
  in
    if fst firstResult then
      let
        secondResult = isPureFunction' functionDeclarations (snd firstResult) secondExpression
      in
        if fst secondResult then
          let
            thirdResult = isPureFunction' functionDeclarations (snd secondResult) thirdExpression
          in
            if fst thirdResult then
              isPureFunction' functionDeclarations (snd thirdResult) body
            else
              thirdResult
        else
          secondResult
    else
      firstResult

-- JSAssignment

isPureFunction' functionDeclarations localDeclarations (JSAssignment (JSVariableDeclaration (JSIdentifier identifier)) rightExpression) =
  let
    result = isPureFunction' functionDeclarations localDeclarations rightExpression
  in
    if fst result then
      (True, (identifier, rightExpression, True):localDeclarations)
    else
      result
isPureFunction' functionDeclarations localDeclarations (JSAssignment leftExpression rightExpression) =
  let
    leftResult = isPureFunction' functionDeclarations localDeclarations leftExpression
  in
    if fst leftResult then
      isPureFunction' functionDeclarations (snd leftResult) rightExpression
    else
      leftResult

-- JSLess

isPureFunction' functionDeclarations localDeclarations (JSLess leftExpression rightExpression) =
  let
    leftResult = isPureFunction' functionDeclarations localDeclarations leftExpression
  in
    if fst leftResult then
      isPureFunction' functionDeclarations (snd leftResult) rightExpression
    else
      leftResult

-- JSOr

isPureFunction' functionDeclarations localDeclarations (JSOr leftExpression rightExpression) =
  let
    leftResult = isPureFunction' functionDeclarations localDeclarations leftExpression
  in
    if fst leftResult then
      isPureFunction' functionDeclarations (snd leftResult) rightExpression
    else
      leftResult

-- JSPostfixPlusUpdate

isPureFunction' functionDeclarations localDeclarations (JSPostfixPlusUpdate expression) = isPureFunction' functionDeclarations localDeclarations expression

-- JSString

isPureFunction' _ localDeclarations (JSString _) = (True, localDeclarations)

-- JSObjectMember

isPureFunction' functionDeclarations localDeclarations (JSObjectMember _ rightExpression) =
  isPureFunction' functionDeclarations localDeclarations rightExpression -- 1 つ目はメンバーの識別子であるためスキップして良い

-- Any

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
