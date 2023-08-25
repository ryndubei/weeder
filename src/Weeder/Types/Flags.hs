{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language DerivingStrategies #-}
{-# language AllowAmbiguousTypes #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
{-# language PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Weeder.Types.Flags (WeederFlag(..), HasFlags(..)) where

import Unsafe.Coerce ( unsafeCoerce )
import Type.Reflection


data WeederFlag
  = TypeClassRoots
  | UnusedTypes
  deriving Eq


class Typeable k => Demote k where
  demote :: TypeRep (a :: k) -> k


instance Demote WeederFlag where
  demote t
    | Just HRefl <- t `eqTypeRep` (typeRep @'TypeClassRoots) = TypeClassRoots
    | Just HRefl <- t `eqTypeRep` (typeRep @'UnusedTypes) = UnusedTypes
    | otherwise = error "Demote WeederFlag: unexpected flag"


instance Demote k => Demote [k] where
  demote t = case t `eqTypeRep` (typeRep @('[] :: [k])) of
    Just HRefl -> []
    Nothing -> case t of
      App (App _cons k) ks -> demote (unsafeCoerce k) : demote (unsafeCoerce ks)
      _ -> error "Demote [k]: impossible"


class Typeable flags => HasFlags (flags :: [WeederFlag]) where
  getFlags :: [WeederFlag]
  getFlags = demote $ typeRep @flags


instance Typeable flags => HasFlags (flags :: [WeederFlag])

type family Elem (a :: k) (as :: [k]) :: Bool where
  Elem a '[] = 'False
  Elem a (a ': as) = 'True
  Elem a (b ': as) = Elem a as
