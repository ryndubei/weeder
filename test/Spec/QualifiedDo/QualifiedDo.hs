{-# LANGUAGE QualifiedDo #-}
module Spec.QualifiedDo.QualifiedDo where

import qualified Spec.QualifiedDo.Foo as Foo

root :: IO ()
root = pure $ Foo.do
  1
  2
