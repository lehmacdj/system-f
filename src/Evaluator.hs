module Evaluator where

import AST

import Data.List ((\\))

import Control.Lens.Plated
import Control.Lens

import Variables

quote :: String -> String
quote s = "\"" ++ s ++ "\""

-- the big step evaluation of a term in the simply typed lambda calculus in a
-- lazy full beta reduction kind of way
evaluate :: Term -> Either String Term
evaluate Unit = Right Unit
evaluate (Var x) = Right $ Var x -- lookup x and return that and x if not present
evaluate (Lam x ty t) = Lam x ty <$> evaluate t
evaluate (App (Lam x _ esub) erep) = pure $ substitute x erep esub
evaluate (App t _) = Left $ quote (show t) ++ " cannot be applied"
