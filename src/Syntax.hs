module Syntax where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (nub, intercalate)

import Text.Printf (printf)

type Var  = String
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

show_term :: Term -> String
show_term Wild = "_"
show_term (TInt n) = show n
show_term (TVar x) = x
show_term (TComp a []) = a
show_term (TComp "|" [x, xs])
  = printf "[%s]" $ show_list x xs
    where 
      show_list x xs =
        case xs of
          TComp "[]" []     -> show_term x
          TComp "|" [y, ys] -> show_term x ++ ", " ++ show_list y ys
          _                 -> show_term x ++ "|" ++ show_term xs
show_term (TComp f ts) =
  printf "%s(%s)" f $ intercalate ", " $ map show_term ts


show_subst :: Subst -> String
show_subst sb =
  if null sb then "true."
  else intercalate ", "
        [printf "%s = %s" x $ show_term t | (x, t) <- Map.toList sb] ++ "."
