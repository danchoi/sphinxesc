{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Main where
import SphinxEscape (escapeSphinxQueryString, parseQuery)
import System.Environment
import Data.List
import Options.Applicative
import Control.Applicative
import Data.Monoid((<>))

data Options = Options {
    optMode :: Mode
  , optInput :: Maybe String
  }

data Mode = Convert | Parse 

mode :: Parser Options
mode = Options
  <$> flag Convert Parse (short 'p' <> help "Show parser evaluation")
  <*> (
          (Just <$> strArgument (metavar "RAW-STRING" <> help "sphinx raw input expression"))
          <|> pure Nothing
      )

opt :: ParserInfo Options
opt = info (helper <*> mode) (header "sphinxesc")
  

main = do
  Options{..} <- execParser opt
  input <- maybe getContents return optInput
  case optMode of 
      Convert -> do
        putStrLn $ escapeSphinxQueryString input
      Parse -> do
        print $ parseQuery input


  
  
