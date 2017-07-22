{-# LANGUAGE MultiParamTypeClasses #-}

module Variables where

class VarContaining t v where
    fvs :: t -> [v]

class VarContaining t v => Substitutable t v where
    substitute :: v -> t -> t -> t
