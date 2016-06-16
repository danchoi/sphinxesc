{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 

module SphinxEscape where
import Text.Parsec
import Control.Applicative
import Text.Parsec hiding (many, (<|>)) 
 

type Parser = ParsecT String () Identity 

runParse parser inp =
  case Text.Parsec.parse parser "" inp of
    Left x -> error $ "parser failed: " ++ show x
    Right xs -> xs


-- | Escapes extended sphinx query
escQuery :: String -> String
escQuery = undefined

-- http://sphinxsearch.com/docs/latest/extended-syntax.html

type Pos = Int
type Proximity = Int


{-
escape single quotes?
@(tag_list) or @tag_list is the only permitted @expr

no trailnig end

eliminate all non alphanum

-}
