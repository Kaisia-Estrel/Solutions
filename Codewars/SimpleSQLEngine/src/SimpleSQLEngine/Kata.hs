{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module SimpleSQLEngine.Kata where

import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, many, manyTill, optional, sepBy1, try, (<?>), (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1)
import Text.Megaparsec.Char.Lexer qualified as T

type Parser = Parsec Void String

space :: Parser ()
space = T.space space1 (T.skipLineComment "--") (T.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = T.lexeme space

symbol :: String -> Parser String
symbol = T.symbol' space

identifier :: Parser String
identifier =
  choice
    [ delimitedIdentifier <?> "delimited_identifier"
    , regularIdentifier <?> "regular_identifier"
    ]
    <?> "identifier"
  where
    regularIdentifier = do
      x <- letterChar <|> char '_' <|> char '@' <|> char '#'
      xs <- many (alphaNumChar <|> char '_' <|> char '_' <|> char '@' <|> char '#')
      return (x : xs)
    delimitedIdentifier = do
      delim <- char '"' <|> char '['
      manyTill T.charLiteral $ case delim of
        '"' -> char '"'
        '[' -> char ']'
        _ -> error "Should Not Occur"

sqlEngine :: [(String, [[(String, String)]])] -> String -> [[(String, String)]]
sqlEngine = undefined
