{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Web.SimpleStaticServer

import Options.Applicative (
  Parser, ParserInfo,
  option, long, short, help, value, showDefault, auto, str,
  info, helper, fullDesc,
  execParser)
import Data.Semigroup ((<>))

data Options = Options { path :: FilePath, port :: Int }

optionParser :: Parser Options
optionParser = Options
  <$> option str ( long "dir" <> short 'd' <> help "Directory to serve" <> value "." <> showDefault)
  <*> option auto ( long "port" <> short 'p' <> help "Port" <> value 80 <> showDefault )

optionParserInfo :: ParserInfo Options
optionParserInfo = info (helper <*> optionParser) fullDesc

main :: IO ()
main = do
  Options{port, path} <- execParser optionParserInfo
  runServer port path
