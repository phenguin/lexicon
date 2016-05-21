module Lexicon.Lisp.Eval where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Typeable
import Data.Fix
import Data.Maybe
import Data.Either

type VarName = Text

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither x my = case my of
  Nothing -> Left x
  Just y -> Right y

newtype Frame = Frame {frameMap :: Map VarName Value}
type Env = [Frame]

data Value = Nil | Symbol Text | Number Integer | Boolean Bool | Proc ([Value] -> Either ErrorMsg Value)
instance Show Value where
  show v = case v of
    Nil -> "nil"
    Symbol text -> show text
    Number n -> show n
    Boolean b -> if b then "#t" else "#f"
    Proc _ -> "<lambda>"
data ExprF a = V VarName
           | N Integer
           | B Bool
           | Lambda [VarName] Expr
           | Begin [a]
           | Cond [(a, a)]
           | If a a a
           | Ap a [a]
           deriving (Eq, Show, Functor, Foldable, Traversable, Typeable)

type Expr = Fix ExprF

lastMay :: [a] -> Maybe a
lastMay xs = case xs of
  [] -> Nothing
  [x] -> Just x
  (_:xs) -> lastMay xs

type ErrorMsg = String

eval' :: Env -> ExprF Value -> Either ErrorMsg Value
eval' env expr = case expr of
           N i -> return $ Number i
           B b -> return $ Boolean b
           V name -> maybeToEither "Variable not found" $ envFind env name
           Lambda vars expr -> return . Proc $ \vals ->
             eval (makeEnv vars vals env) expr
           Begin exprs -> return $ fromMaybe Nil (lastMay exprs)
           Cond pairs -> case pairs of
                           [] -> return Nil
                           ((Boolean b, v):rest) -> if b then return v else eval' env (Cond rest)
                           (_:rest) -> Left $ "Non-boolean Cond condition"
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
