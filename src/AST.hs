module AST where

data Term = Unit
          | Var String
          | Lam String Type Term
          | App Term Term
        -- | TyLam String Term
        deriving (Show)

data Type = UnitTy
          | FunTy Type Type
          deriving (Show, Eq)
