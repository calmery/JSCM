module Types (Declaration) where

import           Parser (Expression)
import           RIO

type Declaration = (String, Expression, Bool) -- (Identifier, Expression, Searched)
