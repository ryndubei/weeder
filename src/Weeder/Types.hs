{-# language TypeFamilies #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language NamedFieldPuns #-}
{-# language ConstraintKinds #-}
{-# language FlexibleContexts #-}
{-# language DataKinds #-}
{-# language GADTs #-}

module Weeder.Types 
  ( Declaration(..)
  , NodeTrait(..)
  , IdentifierTrait(..)
  , WeederAST(..)
  , WeederAST'
  ) 
   where

-- base
import Data.List ( intercalate )
import GHC.Generics ( Generic )

-- containers
import Data.Tree ( Tree )
import Data.Set ( Set )

-- ghc
import GHC.Plugins

-- parallel
import Control.Parallel.Strategies ( NFData )


data Declaration =
  Declaration
    { declModule :: Module
      -- ^ The module this declaration occurs in.
    , declOccName :: OccName
      -- ^ The symbol name of a declaration.
    }
  deriving
    ( Eq, Ord, Generic, NFData )


instance Show Declaration where
  show =
    declarationStableName


declarationStableName :: Declaration -> String
declarationStableName Declaration { declModule, declOccName } =
  let
    namespace
      | isVarOcc declOccName     = "var"
      | isTvOcc declOccName      = "tv"
      | isTcOcc declOccName      = "tc"
      | isDataOcc declOccName    = "data"
      | isDataSymOcc declOccName = "dataSym"
      | otherwise                = "unknown"

    in
    intercalate "$" [ namespace, moduleStableString declModule, "$", occNameString declOccName ]


-- | Traits of 'WeederNode' that Weeder is interested in.
data NodeTrait
  -- top-level nodes
  = Binding
  | ClassDeclaration
  | ClassInstance
  | ClassInstanceStandalone
  | DataDeclaration
  | PatternSynonym
  | RewriteRule
  | TypeFamily
  | TypeInstance
  | TypeSignature
  | TypeSynonym
  -- subnodes
  | Constructor
  | DerivingClause
  deriving ( Eq, Show )


-- | Traits of 'WeederIdentifier' that Weeder is interested in.
data IdentifierTrait
  = Dependency
  | FamilyDeclaration
  | TypeSigDeclaration
  deriving ( Eq, Ord, Show )


-- | Type class defining a simplified 'HieAST' for use by Weeder
class WeederAST a where
  -- | A 'WeederAST' is made up of 'WeederNode's
  data WeederNode a
  toWeederAST :: a -> Tree (WeederNode a)

  -- | A 'WeederNode' may have traits describing how it
  -- should be handled. Normally, it will have just one
  -- trait.
  nodeTraits :: WeederNode a -> Set NodeTrait

  -- | Each 'WeederNode' may contain 'WeederIdentifier's
  data WeederIdentifier a
  toIdents :: WeederNode a -> [WeederIdentifier a]

  -- | A 'WeederIdentifier' may be converted to a 'Declaration'
  toDeclaration :: WeederIdentifier a -> Maybe Declaration

  -- | A 'WeederIdentifier' may possess several 'IdentifierTrait's.
  -- We can therefore filter a 'WeederAST' for identifiers by these
  -- traits.
  identTraits :: WeederIdentifier a -> Set IdentifierTrait

  -- | Summarised information about all 'WeederAST's. Reveals
  -- extra dependencies, such as type class instances.
  type WeederSumInfo a
  collectSumInfo :: [a] -> WeederSumInfo a
  followWithSumInfo :: WeederSumInfo a -> WeederIdentifier a -> Set Declaration

  -- | Whether 'followWithSumInfo' should be called on this identifier.
  -- Should be 'True' for all identifiers where 'followWithSumInfo'
  -- can be non-empty.
  followable :: WeederIdentifier a -> Bool
  followable _ = True

  -- | A 'WeederIdentifier' may have a 'WeederType'
  data WeederType a
  toIdentType :: WeederIdentifier a -> Maybe (WeederType a)

  -- | A 'WeederType' may be collapsed into a set of 'Declaration's,
  -- representing the individual type constructors used.
  typesUsed :: WeederType a -> Set Declaration


-- | Shortcut for 'WeederAST' and some useful constraints on its data families
type WeederAST' a = (WeederAST a, Eq (WeederType a))
