{-# LANGUAGE TypeFamilies #-}
-- | Control version of Types that is not imported
-- All these declarations should show up in the output
module Spec.Types.TypesUnused where

data Modulo1 = Zero

type Number = Double

newtype Vector = MkVector (Double, Double, Double)

type family Family a

type instance Family Int = Bool
