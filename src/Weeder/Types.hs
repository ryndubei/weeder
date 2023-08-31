{-# language TypeFamilies #-}
{-# language DeriveGeneric #-}
{-# language NamedFieldPuns #-}
{-# language ConstraintKinds #-}
{-# language FlexibleContexts #-}
{-# language ApplicativeDo #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language AllowAmbiguousTypes #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}

module Weeder.Types
  ( Declaration(..)
  , NodeTrait(..)
  , IdentifierTrait(..)
  , WeederAST'
  , WeederAST
    ( initialGraph
    , followable
    , followWithSumInfo
    , collectSumInfo
    )
  , WeederNode
  , WeederIdentifier
  , WeederType
  , WeederLocalInfo
  , WeederSumInfo
  , topLevelAnalysis
  , pattern WeederNode
  , nodeTraits
  , nodeLocation
  , nodeIdents
  , subnodes
  , pattern WeederIdentifier
  , declaration
  , identTraits
  , lookupType
  , pattern WeederType
  , typeConstructors
  )
   where

-- algebraic-graphs
import Algebra.Graph ( Graph, stars )

-- base
import Control.Applicative ( Alternative (..) )
import Control.Monad ( mzero, guard, (>=>) )
import Data.Coerce
import Data.Function
import Data.List ( intercalate )
import Data.Maybe
import GHC.Generics ( Generic )

-- containers
import qualified Data.Map.Strict as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Tree ( Tree, rootLabel, subForest )
import qualified Data.Tree as Tree

-- ghc
import GHC.Plugins
    ( isDataOcc
    , isDataSymOcc
    , isTcOcc
    , isTvOcc
    , isVarOcc
    , occNameString
    , moduleStableString
    , OccName
    , Module
    , Outputable(..)
    , unpackFS
    , srcLocLine
    , realSrcSpanStart, Name, nameModule_maybe, nameOccName
    )
import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils
import GHC.Iface.Type

-- parallel
import Control.Parallel.Strategies ( NFData )

-- weeder
import Weeder.Types.Flags



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


nameToDeclaration :: Name -> Maybe Declaration
nameToDeclaration name = do
  m <- nameModule_maybe name
  return Declaration { declModule = m, declOccName = nameOccName name }


-- | Traits of 'WeederNode' that Weeder is interested in.
data NodeTrait
  -- top-level nodes
  = Binding
  | ClassDeclaration
  | ClassInstance
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
  deriving ( Eq, Show, Ord )


-- | Traits of 'WeederIdentifier' that Weeder is interested in.
data IdentifierTrait
  = IsUse
  | IsEvidenceUse
  | IsDeclaration
  | IsTypeDeclaration
  | IsEvidenceBind Declaration
  deriving ( Eq, Ord, Show )


{-# INLINE nodeTraits #-}
{-# INLINE nodeLocation #-}
{-# INLINE nodeIdents #-}
pattern WeederNode :: WeederAST a => Set NodeTrait -> Int -> [WeederIdentifier a] -> [Tree (WeederNode a)] -> Tree (WeederNode a)
pattern WeederNode{nodeTraits, nodeLocation, nodeIdents, subnodes} <-
  (\(Tree.Node n subnodes) -> (toNodeTraits n, toLocation n, toIdents n, subnodes) -> (nodeTraits, nodeLocation, nodeIdents, subnodes))
{-# COMPLETE WeederNode #-}


{-# INLINE declaration #-}
{-# INLINE identTraits #-}
{-# INLINE lookupType #-}
pattern WeederIdentifier :: WeederAST a => Maybe Declaration -> Set IdentifierTrait -> (WeederLocalInfo a -> Maybe (WeederType a)) -> WeederIdentifier a
pattern WeederIdentifier{declaration, identTraits, lookupType} <-
  (\i -> (toDeclaration i, toIdentTraits i, flip lookupIdentType i) -> (declaration, identTraits, lookupType))
{-# COMPLETE WeederIdentifier #-}


{-# INLINE typeConstructors #-}
pattern WeederType :: WeederAST a => Set Declaration -> WeederType a
pattern WeederType{ typeConstructors } <-
  (typeConstructorsIn -> typeConstructors)
{-# COMPLETE WeederType #-}


-- | Type class defining a simplified 'HieAST' for use by Weeder
class WeederAST a where
  -- | A 'WeederAST' is made up of 'WeederNode's
  data WeederNode a
  toWeederAST :: a -> Tree (WeederNode a)

  -- | A 'WeederNode' may have traits describing how it
  -- should be handled. Normally, it will have just one
  -- trait.
  toNodeTraits :: WeederNode a -> Set NodeTrait

  -- | The line number of the node.
  toLocation :: WeederNode a -> Int

  -- | Each 'WeederNode' may contain 'WeederIdentifier's
  data WeederIdentifier a
  toIdents :: WeederNode a -> [WeederIdentifier a]

  -- | A 'WeederIdentifier' may be converted to a 'Declaration'
  toDeclaration :: WeederIdentifier a -> Maybe Declaration

  -- | A 'WeederIdentifier' may possess 'IdentifierTrait's.
  -- We can therefore filter a 'WeederAST' for identifiers by these
  -- traits.
  toIdentTraits :: WeederIdentifier a -> Set IdentifierTrait

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

  type WeederLocalInfo a
  -- | A 'WeederIdentifier' may have a 'WeederType'
  data WeederType a
  lookupIdentType :: WeederLocalInfo a -> WeederIdentifier a -> Maybe (WeederType a)

  -- | A 'WeederType' may be collapsed into a set of 'Declaration's,
  -- representing the individual type constructors used.
  typeConstructorsIn :: WeederType a -> Set Declaration


  -- | Generate an initial graph of the current AST.
  initialGraph :: WeederLocalInfo a -> Tree (WeederNode a) -> Graph Declaration
  initialGraph = initialGraphDefault


initialGraphDefault :: WeederAST a => WeederLocalInfo a -> Tree (WeederNode a) -> Graph Declaration
initialGraphDefault info ast = stars do
    i <- concatMap toIdents $ Tree.flatten ast
    t <- maybe mzero pure (lookupIdentType info i)
    d <- maybe mzero pure (toDeclaration i)
    let ds = typeConstructorsIn t
    guard $ not (Set.null ds)
    pure (d, Set.toList ds)


-- | Shortcut for 'WeederAST' and some useful constraints on its data families
type WeederAST' a = (WeederAST a, Eq (WeederType a), Outputable (WeederType a))


-- | Try to execute some potentially-failing applicative action on nodes in a 
-- tree starting from the top node.
--
-- If the action fails, try it with the node's children. If it succeeds, do
-- not proceed to the node's children.
topLevelAnalysis :: (Alternative m) => (Tree x -> m b) -> Tree x -> m [b]
topLevelAnalysis f n@Tree.Node{subForest} =
  analyseThis <|> fmap concat analyseChildren
  where
    analyseThis = pure <$> f n
    analyseChildren = traverse (\n' -> topLevelAnalysis f n' <|> pure []) subForest


-- Would be nice if we could do something like
-- HasFlags flags => WeederAST (HieAST TypeIndex)
-- but that does not compile even with AllowAmbiguousTypes,
-- so we have to use the WithFlags newtype
type HieWithFlags fs = WithFlags fs (HieAST TypeIndex)


instance Flags fs => (WeederAST (HieWithFlags fs)) where
  newtype WeederNode (HieWithFlags fs) =
    WN (HieAST TypeIndex)

  {-# INLINABLE toWeederAST #-}
  toWeederAST (WithFlags n) = Tree.Node
    { rootLabel = WN n
    , subForest = map (toWeederAST . coerce) (nodeChildren n)
    }

  {-# INLINABLE toNodeTraits #-}
  toNodeTraits (WN (Node{sourcedNodeInfo})) =
    let anns = Set.toList . Set.unions . fmap nodeAnnotations $ getSourcedNodeInfo sourcedNodeInfo
     in Set.fromList $ mapMaybe (toNodeTrait >=> simplify) anns
    where
      simplify = ifFlagElse @fs @'AnalyseTypes
        Just
        \case
          TypeSignature -> Nothing
          TypeInstance -> Nothing
          TypeSynonym -> Nothing
          TypeFamily -> Nothing
          a -> Just a

  {-# INLINABLE toLocation #-}
  toLocation (WN (Node{nodeSpan})) =
    srcLocLine $ realSrcSpanStart nodeSpan

  newtype WeederIdentifier (HieWithFlags fs) =
    WI (Identifier, IdentifierDetails TypeIndex)

  {-# INLINABLE toIdents #-}
  toIdents (WN (Node{sourcedNodeInfo})) =
    let idents = sourcedNodeIdents sourcedNodeInfo
     in coerce (Map.toList idents)

  {-# INLINABLE toDeclaration #-}
  toDeclaration (WI (ident, _)) =
    case ident of
      Right name -> nameToDeclaration name
      Left _ -> Nothing

  {-# INLINABLE toIdentTraits #-}
  toIdentTraits (WI (_, details)) =
    Set.fromList . mapMaybe (toIdentTrait >=> simplifyUnusedTypes >=> simplifyTypeClassRoots) . Set.toList $ identInfo details
    where
      simplifyUnusedTypes = ifFlagElse @fs @'AnalyseTypes
        Just
        \case
          IsTypeDeclaration -> Just IsDeclaration
          a -> Just a

      simplifyTypeClassRoots = ifFlagElse @fs @'AnalyseInstances
        Just
        \case
          IsEvidenceUse -> Nothing
          a -> Just a

  type WeederSumInfo (HieWithFlags fs) = RefMap TypeIndex

  collectSumInfo = ifFlagElse @fs @'AnalyseInstances
    (const mempty)
    (generateReferencesMap . (coerce :: [WithFlags fs a] -> [a]))

  followWithSumInfo rf (WI (Right name, _)) =
    let evidenceInfos = maybe mempty Tree.flatten (getEvidenceTree rf name)
        instEvidenceInfos = evidenceInfos & filter \case
          EvidenceInfo _ _ _ (Just (EvInstBind _ _, ModuleScope, _)) -> True
          _ -> False
        evBindSiteDecls = mapMaybe (nameToDeclaration . evidenceVar) instEvidenceInfos
     in Set.fromList evBindSiteDecls
  followWithSumInfo _ _ = mempty

  {-# INLINABLE followable #-}
  followable i = ifFlagElse @fs @'AnalyseInstances
    (IsEvidenceUse `Set.member` toIdentTraits i)
    False

  type WeederLocalInfo (HieWithFlags fs) = HieFile

  newtype WeederType (HieWithFlags fs) =
    WT HieTypeFix
    deriving Eq

  {-# INLINABLE lookupIdentType #-}
  lookupIdentType hf (WI (_, details )) =
    let ts = hie_types hf
        i = identType details
     in ifFlagElse @fs @'AnalyseTypes (coerce $ fmap (`recoverFullType` ts) i) Nothing

  {-# INLINABLE typeConstructorsIn #-}
  typeConstructorsIn =
    Set.fromList . mapMaybe nameToDeclaration . Set.toList . typeToNames . coerce

  {-# INLINABLE initialGraph #-}
  initialGraph = ifFlagElse @fs @'AnalyseTypes
    initialGraphDefault
    \_ _ -> mempty


instance Outputable (WeederType (HieWithFlags fs)) where
  ppr = pprIfaceSigmaType ShowForAllWhen . hieTypeToIface . coerce


toNodeTrait :: NodeAnnotation -> Maybe NodeTrait
toNodeTrait a = case unNodeAnnotation a of
  ("FunBind", "HsBindLR")    -> Just Binding
  ("PatBind", "HsBindLR")    -> Just Binding
  ("HsRule", "RuleDecl")     -> Just RewriteRule
  ("ClsInstD", "InstDecl")   -> Just ClassInstance
  ("DerivDecl", "DerivDecl") -> Just ClassInstance
    -- ^ standalone deriving
  ("ClassDecl", "TyClDecl")  -> Just ClassDeclaration
  ("DataDecl", "TyClDecl")   -> Just DataDeclaration
  (_, "ConDecl")             -> Just Constructor
  (_, "HsDerivingClause")    -> Just DerivingClause
  ("SynDecl", "TyClDecl")    -> Just TypeSynonym
  ("FamDecl", "TyClDecl")    -> Just TypeFamily
  ("TyFamInstD", "InstDecl") -> Just TypeInstance
  ("TySig", "Sig")           -> Just TypeSignature
  ("PatSynBind", "HsBindLR") -> Just PatternSynonym
  _                          -> Nothing


toIdentTrait :: ContextInfo -> Maybe IdentifierTrait
toIdentTrait x
  | isUse x = Just IsUse
  | isTypeDeclaration x = Just IsTypeDeclaration
  | isDeclaration x = Just IsDeclaration
  | isEvidenceUse x = Just IsEvidenceUse
  | Just c <- getEvidenceBindClass x = Just (IsEvidenceBind c)
  | otherwise = Nothing


getEvidenceBindClass :: ContextInfo -> Maybe Declaration
getEvidenceBindClass (EvidenceVarBind a@EvInstBind{} ModuleScope _) =
  nameToDeclaration (cls a)
getEvidenceBindClass _ = Nothing


isTypeDeclaration :: ContextInfo -> Bool
isTypeDeclaration = \case
  TyDecl -> True
  Decl DataDec _ -> True
  Decl FamDec _ -> True
  Decl SynDec _ -> True
  _ -> False


isUse :: ContextInfo -> Bool
isUse = \case
  Use -> True
  -- not RecFieldMatch and RecFieldDecl because they occur under
  -- data declarations, which we do not want to add as dependencies
  -- because that would make the graph no longer acyclic
  -- RecFieldAssign will be most likely accompanied by the constructor
  RecField RecFieldOcc _ -> True
  _ -> False


isDeclaration :: ContextInfo -> Bool
isDeclaration = \case
  -- Things that count as declarations
  ValBind RegularBind ModuleScope _ -> True
  PatternBind ModuleScope _ _       -> True
  Decl _ _                          -> True
  ClassTyDecl{}                     -> True

  -- Anything else is not a declaration
  _ -> False


unNodeAnnotation :: NodeAnnotation -> (String, String)
unNodeAnnotation (NodeAnnotation x y) = (unpackFS x, unpackFS y)


typeToNames :: HieTypeFix -> Set Name
typeToNames (Roll t) = case t of
  HTyVarTy n -> Set.singleton n

  HAppTy a (HieArgs args) ->
    typeToNames a <> hieArgsTypes args

  HTyConApp (IfaceTyCon{ifaceTyConName}) (HieArgs args) ->
    Set.singleton ifaceTyConName <> hieArgsTypes args

  HForAllTy _ a -> typeToNames a

  HFunTy _mult b c ->
    typeToNames b <> typeToNames c

  HQualTy a b ->
    typeToNames a <> typeToNames b

  HLitTy _ -> mempty

  HCastTy a -> typeToNames a

  HCoercionTy -> mempty
  where
    hieArgsTypes :: [(Bool, HieTypeFix)] -> Set Name
    hieArgsTypes = foldMap (typeToNames . snd) . filter fst
