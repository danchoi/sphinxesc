{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, FlexibleContexts #-} 
module SphinxEscape where
import Control.Applicative
import Data.Functor.Identity (Identity )
import Text.Parsec hiding (many, (<|>)) 
import Data.Char
 

-- Main function
escapeSphinxQueryString :: String -> String
escapeSphinxQueryString s = expressionToString . parseQuery $ s


-- Just a simplified syntax tree. Besides this, all other input has its
-- non-alphanumeric characters stripped, including double and single quotes and
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

-- escapes expression to string to pass to sphinx
expressionToString :: Expression -> String
expressionToString (TagFieldSearch s) = "@tag_list" ++ escapeString s
expressionToString (Literal s) = escapeString s
expressionToString (AndOrExpr And a b) = expressionToString a ++ " & " ++ expressionToString b
expressionToString (AndOrExpr Or a b) = expressionToString a ++ " | " ++ expressionToString b

-- removes all non-alphanumerics from literal strings that could be parsed
-- mistakenly as Sphinx Extended Query operators
escapeString :: String -> String
escapeString s = map (stripAlphaNum) s

stripAlphaNum :: Char -> Char
stripAlphaNum s | isAlphaNum s = s
                | otherwise = ' '


type Parser' = ParsecT String () Identity 


expression :: Parser' Expression
expression = (try andOrExpr) <|> try tagField <|> literal 

tagField :: Parser' Expression
tagField = do
   char '@'
   string "tag_list" <|> string "(tag_list)"
   s <- manyTill anyChar (try literalStop)
   return $ TagFieldSearch s


andOrExpr :: Parser' Expression
andOrExpr = do 
    a <- (try tagField <|> literal)
    x <- try conjExpr
    b <- expression  -- recursion
    return $ AndOrExpr x a b

conjExpr :: Parser' Conj
conjExpr = andExpr <|> orExpr

andExpr :: Parser' Conj
andExpr = mkConjExpr ["and", "AND", "&"] And

orExpr :: Parser' Conj
orExpr = mkConjExpr ["or", "OR", "|"] Or


mkConjExpr :: [String] -> Conj -> Parser' Conj
mkConjExpr xs t = 
    try (many1 space >> choice (map (string . (++" ")) xs))
    >> return t


literalStop :: Parser' ()
literalStop = (choice [ eof
  , lookAhead (tagField >> return ()) 
  , lookAhead (conjExpr >> return ())
  ])
  <?> "literalStop"

literal :: Parser' Expression
literal = Literal <$> manyTill anyChar (try literalStop)


