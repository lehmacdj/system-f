module TyCheck where

import AST

import Control.Arrow
import Control.Lens

import Data.Map (Map)
import qualified Data.Map as M

type TyEnv = Map String Type

typeCheck :: TyEnv -> Term -> Either String Type
typeCheck e Unit = Right UnitTy
typeCheck e (Var x) = maybe (Left $ "variable " ++ x ++ " not in scope")
                            Right
                            (M.lookup x e)
typeCheck e (Lam x ty t) = right (FunTy ty) $ typeCheck (M.insert x ty e) t
typeCheck e (App t1 t2) =
    case (typeCheck e t1, typeCheck e t2) of
      (Right (FunTy ty1 ty2), Right ty1') ->
          if ty1 == ty1'
             then Right ty2
             else Left "application of function failed"
      res -> Left $ unlines $ toListOf (both . _Left) res
