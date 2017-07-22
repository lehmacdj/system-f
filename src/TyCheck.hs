{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module TyCheck where

import AST
import Variables

import Control.Arrow
import Control.Lens

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

type TyEnv = Map Var Type
type KindEnv = Set TyVar

-- instance VarContaining TyEnv Var where fvs = foldl (\a e -> a ++ fvs e) []

typeCoherent :: KindEnv -> Type -> Bool
typeCoherent ke UnitTy = True
typeCoherent ke (TyVar x) = x `S.member` ke
typeCoherent ke (FunTy ty1 ty2) = typeCoherent ke ty1 && typeCoherent ke ty2
typeCoherent ke (Forall x ty) = typeCoherent (S.insert x ke) ty

coherent :: KindEnv -> Type -> Either String Type
coherent ke ty = if typeCoherent ke ty
                          then Right ty
                          else Left $ show ty ++ " is not coherent"

typeCheck :: TyEnv -> KindEnv -> Term -> Either String Type
typeCheck te ke Unit = Right UnitTy
typeCheck te ke (Var x) = maybe
                            (Left $ show x ++ " is not in scope")
                            (coherent ke)
                            (M.lookup x te)
typeCheck te ke (Lam x ty t) =
    ((FunTy ty <$>) . coherent ke)
    =<< typeCheck (M.insert x ty te) ke t
typeCheck te ke (App t1 t2) =
    case (typeCheck te ke t1, typeCheck te ke t2) of
      (Right (FunTy ty1 ty2), Right ty1') ->
          if ty1 == ty1'
             then coherent ke ty2
             else Left "application of function failed"
      res -> Left $ unlines $ toListOf (both . _Left) res
typeCheck te ke (TyLam a t) = Forall a <$> typeCheck te (S.insert a ke) t
typeCheck te ke (TyApp t ty) = do
    ty' <- coherent ke ty
    Forall x ty'' <- typeCheck te ke t
    return $ substitute x ty' ty''
