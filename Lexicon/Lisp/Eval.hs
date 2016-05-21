module Lexicon.Lisp.Eval where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Typeable
import Data.Fix
import Data.Maybe


type VarName = Text

newtype Frame = Frame {frameMap :: Map VarName Value}
type Env = [Frame]

data Value = Nil | Symbol Text | Number Integer | Boolean Bool | Proc ([Value] -> Value)
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

eval' :: Env -> ExprF Value -> Value
eval' env expr = case expr of
           N i -> Number i
           B b -> Boolean b
           V name -> fromMaybe (error "Variable not found") $ envFind env name
           Lambda vars expr -> Proc $ \vals ->
             eval (makeEnv vars vals env) expr
           Begin exprs -> fromMaybe Nil (lastMay exprs)
           Cond pairs -> case pairs of
                           [] -> Nil
                           ((Boolean b, v):rest) -> if b then v else eval' env (Cond rest)
                           (_:rest) -> eval' env (Cond rest)
           If pred tval fval -> case pred of
             Boolean True -> tval
             Boolean False -> fval
             _ -> error "IF error"
           Ap func args -> case func of
             Proc f -> f args
             _ -> error "Tried to apply non-function"

eval :: Env -> Expr -> Value
eval env = cata (eval' env)

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
testEnv = initEnv ["a", "b", "c", "+"] $ map Number [1, 10, 100] ++ map Proc [sumvals]

test :: IO ()
test = print ("hi" :: Text)
