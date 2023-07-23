{-# LANGUAGE GADTs #-}

module Spec.GADTClassAcyclic.GADTClassAcyclic where

data Foo where
  MkFoo :: Bar a => a -> Foo

class Bar a where
  bar :: a -> Foo
