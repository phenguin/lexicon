module Lexicon.Lisp.Eval where

import Data.Text (Text)
-- import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Typeable
import Data.Fix
import Data.Maybe

type VarName = Text

newtype Frame = Frame {frameMap :: Map VarName Value}
type Env = [Frame]

data Value = Symbol Text | Num Integer | Proc ([Value] -> Maybe Value)
data ExprF a = V VarName
           | N Integer
           | B Bool
           | Lambda [VarName] a
           | Begin [a]
           | Cond [(a, a)]
           | If a a a
           deriving (Eq, Show, Read, Functor, Foldable, Traversable, Typeable)

type Expr = Fix ExprF

eval' :: Env -> ExprF Value -> Value
eval' = undefined

makeEnv :: [Text] -> [Value] -> Env -> Env
makeEnv vars vals outer = Frame (M.fromList (zip vars vals)) : outer

initEnv :: [Text] -> [Value] -> Env
initEnv vars vals = makeEnv vars vals []

envFind :: Env -> VarName -> Maybe Value
envFind env var = envFind' env var >>= frameLookup var

envFind' :: Env -> VarName -> Maybe Frame
envFind' [] v = Nothing
envFind' (Frame t:rest) v = case M.lookup v t of
  Nothing -> envFind' rest v
  _ -> Just $ Frame t

frameLookup :: VarName -> Frame -> Maybe Value
frameLookup v (Frame t) = M.lookup v t

test :: IO ()
test = print ("hi" :: Text)
