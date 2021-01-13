module Eval where

import Data.List (nub)
import Data.Map ()
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax
  ( Constr,
    Prog,
    Prop (Prop),
    Query,
    Rule (Rule),
    Subst,
    Term (TComp, TInt, TVar, Wild),
    Var,
  )

answerSub :: Prog -> [(Query, Subst)] -> [Subst]
answerSub _ [] = []
answerSub prog (([], sb) : res) = sb : answerSub prog res
answerSub prog ((q : qs, sb) : res) =
  answerSub prog (res ++ adding)
  where
    adding =
      do
        Rule hd tl <- freshRule (varsQuery (q : qs)) <$> prog
        case unifyProp q hd of
          Nothing -> []
          Just sb' ->
            [(map (substProp sb') (qs ++ tl), compose sb' sb)]

answer :: Prog -> Query -> [Subst]
answer prog query =
  let vars = Set.unions (map varsProp query)
   in nub $
        map (Map.filterWithKey (\k _ -> Set.member k vars)) $
          answerSub prog [(query, Map.empty)]

subst :: Subst -> Term -> Term
subst _ Wild = Wild
subst _ (TInt n) = TInt n
subst sb (TVar v) =
  case Map.lookup v sb of
    Just t -> t
    Nothing -> TVar v
subst dic (TComp f xs) = TComp f $ map (subst dic) xs

substConstr :: Subst -> Constr -> Constr
substConstr sb constr =
  [(subst sb s, subst sb t) | (s, t) <- constr]

substProp :: Subst -> Prop -> Prop
substProp sb (Prop r ts) = Prop r $ map (subst sb) ts

substRule :: Subst -> Rule -> Rule
substRule sb (Rule hd tl) =
  Rule (substProp sb hd) (fmap (substProp sb) tl)

compose :: Subst -> Subst -> Subst
compose sb2 sb1 = Map.union (Map.map (subst sb2) sb1) sb2

appear :: Var -> Term -> Bool
appear _ Wild = False
appear _ (TInt _) = False
appear x (TVar y) = x == y
appear x (TComp _ xs) = any (appear x) xs

varsIn :: Term -> Set Var
varsIn Wild = Set.empty
varsIn (TInt _) = Set.empty
varsIn (TVar x) = Set.singleton x
varsIn (TComp _ ts) = Set.unions (map varsIn ts)

varsProp :: Prop -> Set Var
varsProp (Prop _ ts) = Set.unions (map varsIn ts)

varsQuery :: Query -> Set Var
varsQuery ps = Set.unions (map varsProp ps)

varsRule :: Rule -> Set Var
varsRule (Rule hd tl) = Set.unions (map varsProp (hd : tl))

freshRule :: Set Var -> Rule -> Rule
freshRule taboovars rule =
  let oldvars = Set.toList $ varsRule rule
   in let newtvars =
            take
              (length oldvars)
              [TVar v | i <- [1 :: Integer ..], let v = "X" ++ show i, v `notElem` taboovars]
       in substRule (Map.fromList $ zip oldvars newtvars) rule

unify :: Constr -> Maybe Subst
unify [] = Just Map.empty
unify ((Wild, _) : constr) = unify constr
unify ((_, Wild) : constr) = unify constr
unify ((TVar x, t) : constr)
  | t == TVar x = unify constr
  | appear x t = Nothing
  | otherwise =
    let sb1 = Map.singleton x t
     in do
          sb2 <- unify (substConstr sb1 constr)
          return $ compose sb2 sb1
unify ((t, TVar x) : constr) = unify $ (TVar x, t) : constr
unify ((TComp f xs, TComp g ys) : constr) =
  if f == g && length xs == length ys
    then unify $ zip xs ys ++ constr
    else Nothing
unify _ = Nothing

unifyProp :: Prop -> Prop -> Maybe Subst
unifyProp (Prop r xs) (Prop s ys) =
  if r == s && length xs == length ys
    then unify (zip xs ys)
    else Nothing
