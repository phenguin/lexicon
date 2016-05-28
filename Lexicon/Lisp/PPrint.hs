
module Lexicon.Lisp.PPrint where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Fix
import Lexicon.Lisp.Types
import Data.List (intersperse)

type IndentAmt = Int

unlinesT :: [Text] -> Text
unlinesT = T.concat . intersperse "\n"

(<+>) = T.append
emptyT = ""

dI :: Int
dI = 3

indented :: IndentAmt -> Text -> Text
-- indented ind t = T.replicate ind " " ++ t
indented ind t = unlinesT $ map (T.append (T.replicate ind " ")) (T.lines t)

showT :: (Show a) => a -> Text
showT = T.pack . show

pprint' :: ExprF Text -> Text
pprint' expr = case expr of
  N i -> showT (Number i)
  B b -> showT (Boolean b)
  V s -> showT (Symbol s)
  Lambda vars exp -> "(lambda (" <+> T.unwords vars <+> ")\n" <+> indented dI (pprint exp) <+> ")"
  Begin exprs -> "(begin\n" <+> unlinesT (map (indented dI) exprs) <+> ")"
  Cond pairs -> "(cond\n" <+> unlinesT (map (indented dI . (\(c,v) -> "(" <+> c <+> " " <+> v <+> ")")) pairs) <+> ")"
  If p tv fv -> "(if " <+> p <+> "\n" <+> indented (2 * dI) tv <+> "\n" <+> indented dI fv <+> ")"
  Ap f args -> "(" <+> f <+> "\n" <+> indented dI (unlinesT args) <+> ")"

-- data Fix f = Fix (f (Fix f))

pprint :: Expr -> Text
pprint = cata pprint'
