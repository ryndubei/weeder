module Spec.QualifiedDo.Foo ((>>)) where

import Prelude hiding ((>>))

(>>) :: Int -> Int -> ()
_ >> _ = ()
