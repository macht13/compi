module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok

import Numeric

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = [";","(",")",",","=",":","->","+","*",".",">=","-"]
    names = ["end","return","var","cond","continue","break","not","head","tail","islist","or"]
    style = emptyDef {
               Tok.commentStart = "{"
             , Tok.commentEnd   = "}"
             , Tok.commentLine  = "" -- not supported
             , Tok.nestedComments = False
             , Tok.identStart = letter
             , Tok.identLetter = alphaNum
             , Tok.reservedOpNames = ops
             , Tok.reservedNames = names
             }

{-
Necessary lexers:
- lexeme: ; ( ) , = : -> + * . >= -
- ignore space, \t, \n
-}

num :: Parser Integer
num = dec <|> hex

dec :: Parser Integer
dec = Tok.integer lexer

hex :: Parser Integer
hex = do
  val <- hexStr
  ; return $ fst $ head $ readHex val

hexStr :: Parser String
hexStr = char '$' *> many1 hexDigit

float :: Parser Double
float = Tok.float lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
