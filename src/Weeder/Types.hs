{-# language ViewPatterns #-}
{-# language PatternSynonyms #-}
{-# language ApplicativeDo #-}
{-# language NamedFieldPuns #-}
{-# language DeriveGeneric #-}
{-# language TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-} -- snake_case for internal function synonyms

module Weeder.Types 
  ( -- * Weeder AST
    WeederNode
  , WeederIdentifier
  , WeederType
  , toWeederAST
  , pattern WeederNode
  , nodeAnns
  , nodeLocation
  , nodeIdents
  , pattern WeederIdentifier
  , identName
  , identContext
  , identWeederType
  , pattern WeederType
  , typeConstructorNames
    -- * Utilities
  , ContextInfoMap
  , topLevelAnalysis
  , searchContextInfo
  )
   where

import Data.Tree ( Tree, subForest, rootLabel )
import qualified Data.Tree as Tree
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import qualified Data.DList as DList
import GHC.Plugins hiding ((<>))
import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils
import Data.Coerce
import GHC.Iface.Type
import Control.Monad
import GHC.Generics
import Control.Parallel.Strategies
import Control.Applicative
import Data.Bifunctor
import Control.Monad.Zip


data Declaration =
  Declaration
    { declModule :: Module
      -- ^ The module this declaration occurs in.
    , declOccName :: OccName
      -- ^ The symbol name of a declaration.
    }
  deriving
    ( Eq, Ord, Generic )

instance NFData Declaration


newtype WeederNode = WN (HieAST TypeIndex, HieFile)


newtype WeederIdentifier = WI (Identifier, IdentifierDetails TypeIndex, HieFile)


newtype WeederType = WT HieTypeFix


instance Outputable WeederType where
  ppr = pprIfaceSigmaType ShowForAllWhen . hieTypeToIface . coerce


pattern WeederNode :: Set NodeAnnotation -> Maybe Int -> [WeederIdentifier] -> WeederNode
pattern WeederNode{ nodeAnns, nodeLocation, nodeIdents } <-
  (\n -> (node_anns n, node_location n, node_idents n) -> (nodeAnns, nodeLocation, nodeIdents))
{-# COMPLETE WeederNode #-}


pattern WeederIdentifier :: Maybe Name -> Set ContextInfo -> Maybe WeederType -> WeederIdentifier
pattern WeederIdentifier{ identName, identContext, identWeederType } <-
  (\i -> (ident_name i, ident_context i, lookup_ident_type i) -> (identName, identContext, identWeederType))
{-# COMPLETE WeederIdentifier #-}


pattern WeederType :: [Name] -> WeederType
pattern WeederType{ typeConstructorNames } <-
  (type_constructor_names -> typeConstructorNames )
{-# COMPLETE WeederType #-}


-- | Invariant: the HieAST must come from the given HieFile
toWeederAST :: HieFile -> HieAST TypeIndex -> Tree WeederNode
toWeederAST hf = Tree.unfoldTree (\n@Node{nodeChildren} -> (WN (n, hf), nodeChildren))


-- | Try to execute some potentially-failing applicative action on nodes in a 
-- tree starting from the top node.
--
-- If the action fails, try it on the node's children. If it succeeds, do
-- not proceed to the node's children.
--
-- Returns the parts of the tree that failed, and a list of successful outputs.
--
-- Always succeeds.
topLevelAnalysis :: (Alternative m) => Tree x -> (Tree x -> m b) -> m ([b], Maybe (Tree x))
topLevelAnalysis n@Tree.Node{subForest} f =
  ((,Nothing) <$> analyseThis) <|> fmap (second Just . concatSubforest) analyseSubForest
  where
    concatSubforest = foldr (\(as, s) (bs, t) -> (as ++ bs, t{subForest = maybe subForest (:subForest) s})) ([],n)
    analyseThis = pure <$> f n
    analyseSubForest = traverse (\n' -> topLevelAnalysis n' f <|> pure ([], Just n')) subForest


-- | Find keys and values in a tree.
-- A map on a node contains the union of the maps of its children.
searchTree :: Ord b => (a -> [(b, c)]) -> Tree a -> Tree (Map b [c])
searchTree f = fmap (fmap DList.toList) . toMapTree
  where
    toMapTree = Tree.foldTree $ \n ms ->
      let n' = Map.fromListWith (<>) . map (second pure) $ f n
          ms' = Map.unionsWith (<>) (n':map rootLabel ms)
       in Tree.Node ms' ms


volumiseTree :: Tree a -> Tree (Tree a)
volumiseTree = Tree.unfoldTree (\t -> (t, subForest t))


type ContextInfoMap = Map ContextInfo [(Tree WeederNode, WeederIdentifier)]


-- | Find locations of 'ContextInfo' under every node in the tree.
searchContextInfo :: Tree WeederNode -> Tree (WeederNode, ContextInfoMap)
searchContextInfo ast = mzip ast . searchTree go $ volumiseTree ast
  where
    go n@(rootLabel -> WeederNode{nodeIdents}) =
      map (second (n,))
      $ concatMap (\c@(WeederIdentifier{identContext}) -> map (,c) (Set.toList identContext)) nodeIdents


-- Internals


node_anns :: WeederNode -> Set NodeAnnotation
node_anns (WN (Node{sourcedNodeInfo}, _)) =
  Set.unions . fmap nodeAnnotations $ getSourcedNodeInfo sourcedNodeInfo


node_location :: WeederNode -> Maybe Int
node_location (WN (Node{nodeSpan}, _)) = do
  guard (realSrcSpanStart nodeSpan /= realSrcSpanEnd nodeSpan)
  pure $ srcLocLine (realSrcSpanStart nodeSpan)


node_idents :: WeederNode -> [WeederIdentifier]
node_idents (WN (Node{sourcedNodeInfo}, hf)) =
  let idents = sourcedNodeIdents sourcedNodeInfo
   in coerce . map (uncurry (,,hf)) $ Map.toList idents


ident_name :: WeederIdentifier -> Maybe Name
ident_name (WI (ident, _, _)) =
  case ident of
    Right name -> Just name
    Left _ -> Nothing


ident_context :: WeederIdentifier -> Set ContextInfo
ident_context (WI (_, details, _)) = identInfo details


lookup_ident_type :: WeederIdentifier -> Maybe WeederType
lookup_ident_type (WI (_, details, hf)) = do
  i <- identType details
  let ts = hie_types hf
  pure $ (WT . recoverFullType i) ts


type_constructor_names :: WeederType -> [Name]
type_constructor_names = typeToNames . coerce


-- | Names mentioned within the type.
typeToNames :: HieTypeFix -> [Name]
typeToNames (Roll t) = case t of
  HTyVarTy n -> pure n

  HAppTy a (HieArgs args) ->
    typeToNames a <> hieArgsTypes args

  HTyConApp (IfaceTyCon{ifaceTyConName}) (HieArgs args) ->
    pure ifaceTyConName <> hieArgsTypes args

  HForAllTy _ a -> typeToNames a

  HFunTy _mult b c ->
    typeToNames b <> typeToNames c

  HQualTy a b ->
    typeToNames a <> typeToNames b

  HLitTy _ -> mempty

  HCastTy a -> typeToNames a

  HCoercionTy -> mempty

  where

    hieArgsTypes :: [(Bool, HieTypeFix)] -> [Name]
    hieArgsTypes = foldMap (typeToNames . snd) . filter fst

