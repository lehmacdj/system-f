{-# LANGUAGE DeriveDataTypeable #-}

module AST where

import Data.Data
import Control.Lens.Plated

-- equality is purely structural
data Term = Unit
          | Var String
          | Lam String Type Term
          | App Term Term
        -- | TyLam String Term
        deriving (Show, Data, Typeable, Eq)

instance Plated Term

-- equality is purely structural
data Type = UnitTy
          | FunTy Type Type
        -- | Forall
        -- | Exists
          deriving (Show, Eq, Data, Typeable)
