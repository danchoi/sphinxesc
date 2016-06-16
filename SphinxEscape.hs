{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, FlexibleContexts #-} 

module SphinxEscape where
import Control.Applicative
import Data.Functor.Identity (Identity )
import Text.Parsec hiding (many, (<|>)) 
 
-- Just a simplified syntax tree. Besides this, all other input has it's
-- non-alphanum characters stripped, including double and single quotes and
-- parentheses

data Expression = 
        TagFieldSearch String 
      | Literal String
      | AndOrExpr Conj Expression Expression 
  deriving Show

data Conj = And | Or
  deriving Show

parseQuery :: String -> Expression
parseQuery  inp =
  case Text.Parsec.parse expression "" inp of
    Left x -> error $ "parser failed: " ++ show x
    Right xs -> xs

type Parser' = ParsecT String () Identity 


expression :: Parser' Expression
expression = (try andOrExpr) <|> tagField <|> literal 

tagField :: Parser' Expression
tagField = do
   char '@'
   string "tag_list" <|> string "(tag_list)"
   s <- manyTill anyChar (try literalStop)
   return $ TagFieldSearch s


andOrExpr :: Parser' Expression
andOrExpr = do 
    a <- (tagField <|> literal)
    x <- try conjExpr
    b <- expression  -- recursion
    return $ AndOrExpr x a b

conjExpr :: Parser' Conj
conjExpr = andExpr <|> orExpr

andExpr :: Parser' Conj
andExpr = try (many1 space >> (string "and " <|> string "AND ")) >> return Or

orExpr :: Parser' Conj
orExpr = try (many1 space >> (string "or " <|> string "OR ")) >> return And

literalStop :: Parser' ()
literalStop = (choice [ eof
  , lookAhead (tagField >> return ()) 
  , lookAhead (conjExpr >> return ())
  ])
  <?> "literalStop"

-- | consumes all characters until @, |, or end of text, then strips out all non-alphanumerics
-- before returning value
literal :: Parser' Expression
literal = Literal <$> manyTill anyChar (try literalStop)

type Parser = ParsecT String () Identity 


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
