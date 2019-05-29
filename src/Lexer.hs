module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+","*","-",";"]
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
- number
    - decimal (321...)
    - hex ($Aa...)
- identifier
    - start with letters
    - continue with letters/digits
- keywords:
    - end
    - return
    - var
    - cond
    - continue
    - break
    - not
    - head
    - tail
    - islist
    - or
- lexeme: ; ( ) , = : -> + * . >= -
- ignore space, \t, \n, comments { I am a comment }
-}

integer :: Parser Integer
integer = Tok.integer lexer

dec :: Parser Integer
dec = Tok.integer lexer

parens'  :: Parser ()
parens'  = do {
               char '('
             ; parens'
             ; char ')'
             ; parens'
             }
          <|> return ()

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
