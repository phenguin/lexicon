module Main where

import Lexicon.Lisp.Eval
import Lexicon.Lisp.Parser
import Lexicon.Lisp.PPrint

import qualified Data.Text as T


do_main :: IO String -> IO ()
do_main source = do
  res <- testParseFromInput source
  case res of
    Left err -> print err
    Right expr -> do
      putStrLn . T.unpack $ pprint expr
      case eval testEnv expr of
        Left err -> putStrLn $ "ERROR: " ++ err
        Right result -> putStrLn $ "Eval: " ++ show result


main :: IO ()
main = do_main getContents

mydata = "(cond (#t 5) ((< 5 2) 6))"

testmain = do_main (return mydata)
