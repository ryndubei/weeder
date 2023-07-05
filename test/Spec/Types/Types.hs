{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Spec.Types.Types where

-- this Bounded will not depend on Modulo1 as intended
data Modulo1 = Zero deriving Bounded 

-- should be reachable via Number
data Modulo2 = Zero' | One'

type Number = Modulo2

newtype Vector a = Vector (a, a, a) deriving (Functor)

data Record = Record
  { recordField1 :: Int
  , recordField2 :: Double
  }

type family Family a

type instance Family Int = Record
