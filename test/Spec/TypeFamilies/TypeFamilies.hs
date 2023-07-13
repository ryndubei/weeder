{-# LANGUAGE TypeFamilies #-}

module Spec.TypeFamilies.TypeFamilies where

type family Family a

type instance Family Int = Bool

type instance Family Bool = Int
