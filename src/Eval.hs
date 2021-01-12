module Eval where

import Data.List (intercalate, nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax

answer_sub :: Prog -> [(Query, Subst)] -> [Subst]
answer_sub prog [] = []
answer_sub prog (([], sb) : res) = sb : answer_sub prog res
answer_sub prog ((q : qs, sb) : res) =
  answer_sub prog (res ++ adding)
  where
    adding =
      do
        Rule hd tl <- fresh_rule (vars_query (q : qs)) <$> prog
        case unify_prop q hd of
          Nothing -> []
          Just sb' ->
            [(map (subst_prop sb') (qs ++ tl), compose sb' sb)]

answer :: Prog -> Query -> [Subst]
answer prog query =
  let vars = Set.unions (map vars_prop query)
   in nub $
        map (\sb -> Map.filterWithKey (\k a -> Set.member k vars) sb) $
          answer_sub prog [(query, Map.empty)]

subst :: Subst -> Term -> Term
subst sb Wild = Wild
subst sb (TInt n) = (TInt n)
subst sb (TVar v) =
  case Map.lookup v sb of
    Just t -> t
    Nothing -> TVar v
subst dic (TComp f xs) = TComp f $ map (subst dic) xs

subst_constr :: Subst -> Constr -> Constr
subst_constr sb constr =
  [(subst sb s, subst sb t) | (s, t) <- constr]

subst_prop :: Subst -> Prop -> Prop
subst_prop sb (Prop r ts) = Prop r $ map (subst sb) ts

subst_rule :: Subst -> Rule -> Rule
subst_rule sb (Rule hd tl) =
  Rule (subst_prop sb hd) (fmap (subst_prop sb) tl)

compose :: Subst -> Subst -> Subst
compose sb2 sb1 = Map.union (Map.map (subst sb2) sb1) sb2

appear :: Var -> Term -> Bool
appear x Wild = False
appear x (TInt _) = False
appear x (TVar y) = (x == y)
appear x (TComp _ xs) = any (\t -> appear x t) xs

vars :: Term -> Set Var
vars Wild = Set.empty
vars (TInt _) = Set.empty
vars (TVar x) = Set.singleton x
vars (TComp f ts) = Set.unions (map vars ts)

vars_prop :: Prop -> Set Var
vars_prop (Prop r ts) = Set.unions (map vars ts)

vars_query :: Query -> Set Var
vars_query ps = Set.unions (map vars_prop ps)

vars_rule :: Rule -> Set Var
vars_rule (Rule hd tl) = Set.unions (map vars_prop (hd : tl))

fresh_rule :: Set Var -> Rule -> Rule
fresh_rule taboovars rule =
  let oldvars = Set.toList $ vars_rule rule
   in let newtvars =
            take
              (length oldvars)
              [TVar v | i <- [1 ..], let v = "X" ++ (show i), notElem v taboovars]
       in subst_rule (Map.fromList $ zip oldvars newtvars) rule

unify :: Constr -> Maybe Subst
unify [] = Just Map.empty
unify ((Wild, t) : constr) = unify constr
unify ((t, Wild) : constr) = unify constr
unify ((TVar x, t) : constr) =
  if t == TVar x
    then unify constr
    else
      if appear x t
        then Nothing
        else
          let sb1 = Map.singleton x t
           in do
                sb2 <- unify (subst_constr sb1 constr)
                return $ compose sb2 sb1
unify ((t, TVar x) : constr) = unify $ (TVar x, t) : constr
unify ((TComp f xs, TComp g ys) : constr) =
  if f == g && length xs == length ys
    then unify $ (zip xs ys) ++ constr
    else Nothing
unify _ = Nothing

unify_prop :: Prop -> Prop -> Maybe Subst
unify_prop (Prop r xs) (Prop s ys) =
  if r == s && length xs == length ys
    then unify $ (zip xs ys)
    else Nothing
