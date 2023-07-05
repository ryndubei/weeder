{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Spec.Types.Usages where

import Spec.Types.Types

numberUsage :: Number -> Number
numberUsage = id

familyUsage :: Family Int -> Bool
familyUsage = const True

-- should depend on Modulo1
modulo1Usage = minBound

-- exists to force type inference of modulo1Root to Modulo1
notRoot :: Modulo1
notRoot = modulo1Usage

recordUsage = recordField1

vectorUsage = mapVector (const (0 :: Integer))

mapVector :: (a -> b) -> Vector a -> Vector b
mapVector = fmap
