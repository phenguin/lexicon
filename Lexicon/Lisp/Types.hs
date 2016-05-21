module Lexicon.Lisp.Types where

import Data.Fix
import Data.Text (Text)
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
