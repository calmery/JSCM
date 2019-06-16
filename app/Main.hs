module Main where

import           Convert             (convert)
import           Options.Applicative as O
import           Parser              (parse)
import           RIO                 hiding (hClose)
import           RIO.List.Partial    (head)
import           System.Environment  (getArgs)
import           System.IO           (IOMode (ReadMode), hClose, hGetContents,
                                      openFile, putStrLn)
import           ToString            (toString)

data Option = Option {
  dump  :: Bool,
  input :: String
}

main :: IO ()
main = do
  options <- O.execParser $ withInfo optionParser
  handle <- openFile (input options) ReadMode
  contents <- hGetContents handle
  putStrLn $ case parse (input options) contents of
    Left e       -> show e
    Right result -> if dump options then
      show result
    else
      toString $ convert result
  hClose handle

optionParser :: O.Parser Option
optionParser = Option
  <$> dumpParser
  <*> inputParser
  where
    dumpParser =
      O.switch ( O.long "dump" <> O.short 'd' <> O.help "Show AST" )
    inputParser =
      O.strOption ( O.long "input"  <> metavar "FILE" <> O.short 'i' <> O.help "Input file name" )

withInfo :: O.Parser a -> O.ParserInfo a
withInfo opts = O.info (O.helper <*> opts) description
  where
    description = O.fullDesc
      <> O.header "jscm - JavaScript compiler for multithreading"
