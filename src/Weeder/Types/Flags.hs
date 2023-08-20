{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language GeneralisedNewtypeDeriving #-}
{-# language StandaloneDeriving #-}
{-# language FlexibleContexts #-}
{-# language DerivingStrategies #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language AllowAmbiguousTypes #-}
{-# language LambdaCase #-}

module Weeder.Types.Flags (WeederFlag(..), WithFlag(..), WithFlags, pattern WithFlags) where

import Weeder.Types
import GHC.Plugins ( Outputable )
import Data.Coerce
import qualified Data.Set as Set
import Unsafe.Coerce ( unsafeCoerce ) -- needed for the WithFlags pattern


data WeederFlag
  = TypeClassRoots
  | NoTypes


type family WithFlags (flags :: [WeederFlag]) ast where
  WithFlags '[] ast = ast
  WithFlags (x ': xs) ast = WithFlag x (WithFlags xs ast)


newtype WithFlag (flag :: WeederFlag) ast = WithFlag ast


pattern WithFlags :: ast -> WithFlags a ast
pattern WithFlags ast <- (unsafeCoerce -> ast)
  where WithFlags ast = unsafeCoerce ast


deriving newtype instance Eq (WeederType ast) => Eq (WeederType (WithFlag 'TypeClassRoots ast))
deriving newtype instance Outputable (WeederType ast) => Outputable (WeederType (WithFlag 'TypeClassRoots ast))


deriving newtype instance Eq (WeederType ast) => Eq (WeederType (WithFlag 'NoTypes ast))
deriving newtype instance Outputable (WeederType ast) => Outputable (WeederType (WithFlag 'NoTypes ast))


instance WeederAST ast => WeederAST (WithFlag 'TypeClassRoots ast) where
  newtype (WeederNode (WithFlag 'TypeClassRoots ast)) = WNTCR (WeederNode ast)
  newtype (WeederIdentifier (WithFlag 'TypeClassRoots ast)) = WITCR (WeederIdentifier ast)
  newtype (WeederType (WithFlag 'TypeClassRoots ast)) = WTTCR (WeederType ast)
  type WeederSumInfo (WithFlag 'TypeClassRoots ast) = WeederSumInfo ast
  type WeederLocalInfo (WithFlag 'TypeClassRoots ast) = WeederLocalInfo ast

  -- boilerplate
  toWeederAST = coerce . toWeederAST . coerce
  nodeTraits = nodeTraits . coerce
  nodeLocation = nodeLocation . coerce
  toIdents = coerce . toIdents . coerce
  toDeclaration = toDeclaration . coerce
  collectSumInfo = collectSumInfo . (coerce :: [WithFlag 'TypeClassRoots ast] -> [ast])
  followWithSumInfo info = coerce . followWithSumInfo info . coerce
  lookupType info = coerce . lookupType info . coerce
  typesUsed = typesUsed . coerce

  -- overrides
  identTraits = Set.delete IsEvidenceUse . identTraits . coerce

instance WeederAST ast => WeederAST (WithFlag 'NoTypes ast) where
  newtype (WeederNode (WithFlag 'NoTypes ast)) = WNNT (WeederNode ast)
  newtype (WeederIdentifier (WithFlag 'NoTypes ast)) = WINT (WeederIdentifier ast)
  newtype (WeederType (WithFlag 'NoTypes ast)) = WTNT (WeederType ast)
  type WeederSumInfo (WithFlag 'NoTypes ast) = WeederSumInfo ast
  type WeederLocalInfo (WithFlag 'NoTypes ast) = WeederLocalInfo ast

  -- boilerplate
  toWeederAST = coerce . toWeederAST . coerce
  nodeTraits = nodeTraits . coerce
  nodeLocation = nodeLocation . coerce
  toIdents = coerce . toIdents . coerce
  toDeclaration = toDeclaration . coerce
  collectSumInfo = collectSumInfo . (coerce :: [WithFlag 'NoTypes ast] -> [ast])
  followWithSumInfo info = coerce . followWithSumInfo info . coerce

  -- overrides
  lookupType _ = const Nothing
  typesUsed = mempty
  initialGraph _ = const mempty
  identTraits = Set.map (\case IsTypeDeclaration -> IsDeclaration; x -> x) . identTraits . coerce
