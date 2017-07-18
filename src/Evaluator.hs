module Evaluator where

import AST

import Data.List ((\\))

import Control.Lens.Plated
import Control.Lens

quote :: String -> String
quote s = "\"" ++ s ++ "\""

varStream :: [String]
varStream = letters ++ (varStream >>= appendEachLetter)
    where appendEachLetter s = map (s++) letters
          letters = map (:[]) ['a'..'z']

fresh :: [String] -> String
fresh = head . flip filter varStream . flip ((not.) . elem)

-- the big step evaluation of a term in the simply typed lambda calculus in a
-- lazy full beta reduction kind of way
evaluate :: Term -> Either String Term
evaluate Unit = Right Unit
evaluate (Var x) = Right $ Var x -- lookup x and return that and x if not present
evaluate (Lam x ty t) = Lam x ty <$> evaluate t
evaluate (App (Lam x _ esub) erep) = pure $ substitute x erep esub
evaluate (App t _) = Left $ quote (show t) ++ " cannot be applied"

fvs :: Term -> [String]
fvs (Lam x _ t) = fvs t \\ [x]
fvs (Var x) = [x]
fvs t = concat [fvs t' | t' <- children t]

substitute :: String -> Term -> Term -> Term
substitute x rep (Var y) | x == y    = rep
                         | otherwise = Var y
substitute x rep (s `App` t) =
    let subst = substitute x rep
     in subst s `App` subst t
substitute x rep (Lam y ty t)
  | x == y = Lam y ty t
  | x /= y && y `notElem` fvs rep = Lam y ty (substitute x rep t)
  | x /= y && x `elem` fvs rep =
      Lam y ty $ substitute x rep t'
          where z = fresh (fvs rep)
                t' = substitute y (Var z) t
