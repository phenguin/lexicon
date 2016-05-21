module Lexicon.Lisp.Parser where

import Text.Parsec
import Text.Parsec.Text
import Control.Applicative hiding (many)
import Data.Text (pack, unpack)

import Lexicon.Lisp.Types

lparen :: Parser Char
lparen = char '('

rparen :: Parser Char
rparen = char ')'

idsymbol = oneOf "!$%&|*+-/:<=>?@^_~"

identifier :: Parser VarName
identifier = do
  first <- choice [letter, idsymbol]
  rest <- many (choice [alphaNum, idsymbol])
  return $ pack (first:rest)

varExpr :: Parser Expr
varExpr = v <$> identifier

numberExpr :: Parser Expr
numberExpr = n . read <$> many1 digit

boolExpr :: Parser Expr
boolExpr = do
  _ <- char '#'
  v <- oneOf "tf"
  return . b $ case v of
    't' -> True
    'f' -> False
    _ -> error "Shouldn't get here"

sexp = between lparen rparen . try

lambdaExpr :: Parser Expr
lambdaExpr = between lparen rparen $ do
  string "lambda"
  spaces
  args <- between lparen rparen $ identifier `sepBy` spaces
  spaces
  exp <- expr
  return $ lambda args exp

apExpr :: Parser Expr
apExpr = between lparen rparen $ do
  funcExpr <- expr
  spaces
  rest <- expr `sepBy` spaces
  return $ ap funcExpr rest

ifExpr :: Parser Expr
ifExpr = between lparen rparen $ do
  string "if"
  spaces
  pred <- expr
  spaces
  tbranch <- expr
  spaces
  fbranch <- expr
  return $ iff pred tbranch fbranch

beginExpr :: Parser Expr
beginExpr = between lparen rparen $ do
  string "begin"
  spaces
  begin <$> expr `sepBy` spaces

condExpr :: Parser Expr
condExpr =  between lparen rparen $ do
  string "cond"
  spaces
  let condition = between lparen rparen $ do
        test <- expr
        spaces
        result <- expr
        return (test, result)
  cond <$> condition `sepBy` spaces

expr :: Parser Expr
expr = choice $ map try [lambdaExpr, beginExpr, condExpr, ifExpr, boolExpr, numberExpr, varExpr, apExpr]

testParse s = parse expr "" s

testParseFromInput :: IO (Either ParseError Expr)
testParseFromInput = do
  toParse <- pack <$> getContents
  return $ testParse toParse
