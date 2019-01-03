module TokenParser (tokenParser) where

import           RIO
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.Token    (LanguageDef, TokenParser, commentEnd,
                                       commentLine, commentStart,
                                       makeTokenParser, reservedNames,
                                       reservedOpNames)

keywords :: [String]
keywords =
  [ "true"
  , "false"
  , "while"
  , "continue"
  , "break"
  , "if"
  , "else"
  , "for"
  , "try"
  , "catch"
  , "switch"
  , "case"
  , "default"
  , "var"
  , "function"
  , "return"
  ]

operatorNames :: [String]
operatorNames =
  [ "+"
  , "-"
  , "*"
  , "/"
  , "%"
  , "**"
  , "=="
  , "!="
  , "==="
  , "!=="
  , ">"
  , ">="
  , "<"
  , "<="
  , "="
  , "&&"
  , "||"
  , "!"
  , "++"
  , "--"
  , "&"
  , "|"
  ]

languageDef :: LanguageDef st
languageDef = emptyDef
  { commentStart = "/*"
  , commentEnd = "*/"
  , commentLine = "//"
  , reservedNames = keywords
  , reservedOpNames = operatorNames
  }

tokenParser :: TokenParser ()
tokenParser = makeTokenParser languageDef
