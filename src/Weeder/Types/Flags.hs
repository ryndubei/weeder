{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
{-# language AllowAmbiguousTypes #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
{-# language PolyKinds #-}
{-# language TypeOperators #-}
{-# language ConstraintKinds #-}
{-# language RankNTypes #-}

module Weeder.Types.Flags (WeederFlag(..), Flags, HasFlag, HasNoFlag, ifFlagElse, WithFlags(..)) where

import Type.Reflection ( Typeable, (:~:)(Refl) )
import Data.Data ( eqT )


data WeederFlag
  = AnalyseInstances
  | AnalyseTypes


-- | Constraint with information about which flags are set.
type Flags flags = 
  ( Typeable (Elem 'AnalyseInstances flags)
  , Typeable (Elem 'AnalyseTypes flags)
  )


type HasFlag flags flag = (Flags flags, Elem flag flags ~ 'True)
type HasNoFlag flags flag = (Flags flags, Elem flag flags ~ 'False)


newtype WithFlags (flags :: [WeederFlag]) a = WithFlags a


-- | If the flag is set, run the first argument, otherwise run the second.
{-# INLINE ifFlagElse #-}
ifFlagElse :: forall flags flag a. (Flags flags, Typeable (Elem flag flags)) => (HasFlag flags flag => a) -> a -> a
ifFlagElse yes no = case eqT @'True @(Elem flag flags) of
  Just Refl -> yes
  Nothing -> no


type family Elem (a :: k) (as :: [k]) :: Bool where
  Elem a '[] = 'False
  Elem a (a ': as) = 'True
  Elem a (b ': as) = Elem a as
