{-# LANGUAGE MultiParamTypeClasses #-}

module Variables where

class VarContaining t v where
    fvs :: t -> [v]

-- Substitutable v s t means that v can be substituted for s in t
class VarContaining t v => Substitutable v s t where
    -- substitute v s t computes t{s/v}
    substitute :: v -> s -> t -> t
