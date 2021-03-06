module Lexicon.Lisp.Eval where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Fix
import Data.Maybe

import Lexicon.Lisp.Types

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither x my = case my of
  Nothing -> Left x
  Just y -> Right y

newtype Frame = Frame {frameMap :: Map VarName Value}
type Env = [Frame]

lastMay :: [a] -> Maybe a
lastMay xs = case xs of
  [] -> Nothing
  [x] -> Just x
  (_:xs) -> lastMay xs

eval' :: Env -> ExprF Value -> Either ErrorMsg Value
eval' env expr = case expr of
           N i -> return $ Number i
           B b -> return $ Boolean b
           V name -> maybeToEither ("Variable not found " ++ (T.unpack name)) $ envFind env name
           Lambda vars expr -> return . Proc $ \vals ->
             eval (makeEnv vars vals env) expr
           Begin exprs -> return $ fromMaybe Nil (lastMay exprs)
           Cond pairs -> case pairs of
                           [] -> return Nil
                           ((Boolean b, v):rest) -> if b then return v else eval' env (Cond rest)
                           _ -> Left $ "Non-boolean Cond condition"
           If pred tval fval -> case pred of
             Boolean True -> return tval
             Boolean False -> return fval
             _ -> Left "Non boolean valued if statement predicate"
           Ap func args -> case func of
             Proc f -> f args
             _ -> Left "Tried to apply non-function"

eval :: Env -> Expr -> Either ErrorMsg Value
eval env = cataM (eval' env)

makeEnv :: [VarName] -> [Value] -> Env -> Env
makeEnv vars vals outer = Frame (M.fromList (zip vars vals)) : outer

initEnv :: [VarName] -> [Value] -> Env
initEnv vars vals = makeEnv vars vals []

envFind :: Env -> VarName -> Maybe Value
envFind env var = envFind' env var >>= frameLookup var

envFind' :: Env -> VarName -> Maybe Frame
envFind' [] _ = Nothing
envFind' (Frame t:rest) v = case M.lookup v t of
  Nothing -> envFind' rest v
  _ -> Just $ Frame t

frameLookup :: VarName -> Frame -> Maybe Value
frameLookup v (Frame t) = M.lookup v t

-- TEST STUFF
sumvals :: [Value] -> Value
sumvals vs = Number . sum . catMaybes . map pred $ vs
  where pred (Number x) = Just x
        pred _ = Nothing

testEnv :: Env
testEnv = initEnv ["a", "b", "c", "+"] $ map Number [1, 10, 100] ++ map (Proc . (return .)) [sumvals]

test :: IO ()
test = print ("hi" :: Text)
