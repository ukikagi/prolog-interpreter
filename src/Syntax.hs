module Syntax where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf (printf)

type Var = String

type Atom = String

data Term = Wild | TInt Int | TVar Var | TComp Atom [Term]
  deriving (Eq)

data Prop = Prop Atom [Term]
  deriving (Show, Eq)

data Rule = Rule Prop [Prop]
  deriving (Show, Eq)

type Prog = [Rule]

type Query = [Prop]

type Constr = [(Term, Term)]

type Subst = Map Var Term

instance Show Term where
  show Wild = "Wild"
  show (TInt n) = printf "(TInd %d)" n
  show (TVar x) = printf "(TVar %s)" x
  show (TComp "|" [x, y]) = printf "(tCons %s %s)" (show x) (show y)
  show (TComp "[]" []) = "tNil"
  show (TComp a []) = printf "(tAtom %s)" a
  show (TComp f ts) = printf "(TComp %s %s)" f (show ts)

tAtom :: Atom -> Term
tAtom a = TComp a []

tNil :: Term
tNil = TComp "[]" []

tCons :: Term -> Term -> Term
tCons x xs = TComp "|" [x, xs]

showTerm :: Term -> String
showTerm Wild = "_"
showTerm (TInt n) = show n
showTerm (TVar x) = x
showTerm (TComp a []) = a
showTerm (TComp "|" [x, xs]) =
  printf "[%s]" $ show_list x xs
  where
    show_list y ys =
      case ys of
        TComp "[]" [] -> showTerm y
        TComp "|" [z, zs] -> showTerm y ++ ", " ++ show_list z zs
        _ -> showTerm y ++ "|" ++ showTerm ys
showTerm (TComp f ts) =
  printf "%s(%s)" f $ intercalate ", " $ map showTerm ts

showSubst :: Subst -> String
showSubst sb =
  if null sb
    then "true."
    else
      intercalate
        ", "
        [printf "%s = %s" x $ showTerm t | (x, t) <- Map.toList sb]
        ++ "."
