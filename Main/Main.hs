module Main where

import Lexicon.Lisp.Eval
import Lexicon.Lisp.Parser

main :: IO ()
main = do
  res <- testParseFromInput
  case res of
    Left err -> print err
    Right expr -> case eval testEnv expr of
      Left err -> putStrLn $ "ERROR: " ++ err
      Right result -> putStrLn $ "Result: " ++ show result
