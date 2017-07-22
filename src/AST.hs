{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, FlexibleContexts,
             MultiParamTypeClasses #-}

module AST where

import Data.Data
import Control.Lens.Plated
import Data.List ((\\))

import Variables
import Unique

newtype Var = Variable String deriving (Eq, Show, Data, Typeable, Ord)

instance Uniquable Var where
    ustream = fmap Variable ustream

newtype TyVar = TyVariable String deriving (Eq, Show, Data, Typeable, Ord)

instance Uniquable TyVar where
    ustream = fmap TyVariable ustream

-- equality is purely structural
data Term = Unit
          | Var Var
          | Lam Var Type Term
          | App Term Term
          | TyLam TyVar Term
          | TyApp Term Type
        deriving (Show, Data, Typeable, Eq)

var :: String -> Term
var = Var . Variable

instance Plated Term

instance VarContaining Term Var where
    fvs (Lam x _ t) = fvs t \\ [x]
    fvs (Var x) = [x]
    fvs t = concat [fvs t' | t' <- children t]

instance VarContaining Term TyVar where
    fvs (TyApp t ty) = fvs t ++ fvs ty
    fvs (TyLam a t) = fvs t \\ [a]
    fvs t = concat [fvs t' | t' <- children t]

instance Substitutable Term Var where
    substitute x rep Unit = Unit
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
              where z = runUnique fresh (fvs rep :: [Var])
                    t' = substitute y (Var z) t
    substitute x rep (TyLam a t) = TyLam a (substitute x rep t)
    substitute x rep (TyApp t ty) = TyApp (substitute x rep t) ty

-- equality is purely structural
data Type = UnitTy
          | TyVar TyVar
          | FunTy Type Type
          | Forall TyVar Type
        -- | Exists
          deriving (Show, Eq, Data, Typeable)

tyVar :: String -> Type
tyVar = TyVar . TyVariable

instance Plated Type

instance VarContaining Type TyVar where
    fvs UnitTy = []
    fvs (TyVar x) = [x]
    fvs (FunTy ty1 ty2) = fvs ty1 ++ fvs ty2
    fvs (Forall x ty) = fvs ty \\ [x]

instance Substitutable Type TyVar where
    substitute x rep UnitTy = UnitTy
    substitute x rep (TyVar y) | x == y = rep
                               | otherwise = TyVar y
    substitute x rep (FunTy ty1 ty2) =
        let subst = substitute x rep
         in FunTy (subst ty1) (subst ty2)
    substitute x rep (Forall y ty)
      | x == y = Forall y ty
      | x /= y && y `notElem` fvs rep = Forall y (substitute x rep ty)
      | x /= y && x `elem` fvs rep =
          Forall y $ substitute x rep ty
              where z = runUnique fresh (fvs rep :: [TyVar])
                    ty' = substitute y (TyVar z) ty
