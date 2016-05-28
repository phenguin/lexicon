module Lexicon.Lisp.Types where

import Data.Fix
import Data.Text (Text, unpack)
import Data.Typeable

type VarName = Text

data ExprF a = V VarName
           | N Integer
           | B Bool
           | Lambda [VarName] Expr
           | Begin [a]
           | Cond [(a, a)]
           | If a a a
           | Ap a [a]
           deriving (Eq, Show, Functor, Foldable, Traversable, Typeable)

v = Fix . V
n = Fix . N
b = Fix . B

lambda vs e = Fix (Lambda vs e)
begin = Fix . Begin
cond = Fix . Cond
iff p t f  = Fix $ If p t f
ap f args = Fix $ Ap f args

type Expr = Fix ExprF

type ErrorMsg = String

data Value = Nil | Symbol Text | Number Integer | Boolean Bool | Proc ([Value] -> Either ErrorMsg Value)
instance Show Value where
  show v = case v of
    Nil -> "nil"
    Symbol text -> unpack text
    Number n -> show n
    Boolean b -> if b then "#t" else "#f"
    Proc _ -> "<lambda>"
