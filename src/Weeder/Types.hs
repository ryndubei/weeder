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

module Weeder.Types
  ( Declaration(..)
  , NodeTrait(..)
  , IdentifierTrait(..)
  , WeederAST(..)
  , WeederAST'
  , topLevelAnalysis
  , analysisByCases
  )
   where

-- algebraic-graphs
import Algebra.Graph ( Graph, stars )

-- base
import Control.Applicative ( Alternative (..), asum )
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


-- | Type class defining a simplified 'HieAST' for use by Weeder
class WeederAST a where
  -- | A 'WeederAST' is made up of 'WeederNode's
  data WeederNode a
  toWeederAST :: a -> Tree (WeederNode a)

  -- | A 'WeederNode' may have traits describing how it
  -- should be handled. Normally, it will have just one
  -- trait.
  nodeTraits :: WeederNode a -> Set NodeTrait

  -- | The line number of the node.
  nodeLocation :: WeederNode a -> Int

  -- | Each 'WeederNode' may contain 'WeederIdentifier's
  data WeederIdentifier a
  toIdents :: WeederNode a -> [WeederIdentifier a]

  -- | A 'WeederIdentifier' may be converted to a 'Declaration'
  toDeclaration :: WeederIdentifier a -> Maybe Declaration

  -- | A 'WeederIdentifier' may possess 'IdentifierTrait's.
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

  type WeederLocalInfo a
  -- | A 'WeederIdentifier' may have a 'WeederType'
  data WeederType a
  lookupType :: WeederLocalInfo a -> WeederIdentifier a -> Maybe (WeederType a)

  -- | A 'WeederType' may be collapsed into a set of 'Declaration's,
  -- representing the individual type constructors used.
  typeConstructorsIn :: WeederType a -> Set Declaration


  -- | Generate an initial graph of the current AST.
  initialGraph :: WeederLocalInfo a -> Tree (WeederNode a) -> Graph Declaration
  initialGraph = initialGraphDefault


initialGraphDefault :: WeederAST a => WeederLocalInfo a -> Tree (WeederNode a) -> Graph Declaration
initialGraphDefault info ast = stars do
    i <- concatMap toIdents $ Tree.flatten ast
    t <- maybe mzero pure (lookupType info i)
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


-- | 'topLevelAnalysis' but specialised to functions also taking a 'NodeTrait'.
-- The first successful action on each node is used.
analysisByCases
  :: (WeederAST a, Alternative m)
  => (NodeTrait -> Tree (WeederNode a) -> m b)
  -> Tree (WeederNode a)
  -> m [b]
analysisByCases f n@Tree.Node{rootLabel} =
  let traits = Set.toList $ nodeTraits rootLabel
      f' x = asum $ map (`f` x) traits
   in topLevelAnalysis f' n


-- Would be nice if we could do something like
-- HasFlags flags => WeederAST (HieAST TypeIndex)
-- but that does not compile even with AllowAmbiguousTypes,
-- so we have to use the WithFlags newtype
type HieWithFlags fs = WithFlags fs (HieAST TypeIndex)


instance Flags fs => (WeederAST (HieWithFlags fs)) where
  newtype WeederNode (HieWithFlags fs) =
    WeederNode (HieAST TypeIndex)

  {-# INLINABLE toWeederAST #-}
  toWeederAST (WithFlags n) = Tree.Node
    { rootLabel = WeederNode n
    , subForest = map (toWeederAST . coerce) (nodeChildren n)
    }

  {-# INLINABLE nodeTraits #-}
  nodeTraits (WeederNode (Node{sourcedNodeInfo})) =
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

  {-# INLINABLE nodeLocation #-}
  nodeLocation (WeederNode (Node{nodeSpan})) =
    srcLocLine $ realSrcSpanStart nodeSpan

  newtype WeederIdentifier (HieWithFlags fs) =
    WeederIdentifier (Identifier, IdentifierDetails TypeIndex)

  {-# INLINABLE toIdents #-}
  toIdents (WeederNode (Node{sourcedNodeInfo})) =
    let idents = sourcedNodeIdents sourcedNodeInfo
     in coerce (Map.toList idents)

  {-# INLINABLE toDeclaration #-}
  toDeclaration (WeederIdentifier (ident, _)) =
    case ident of
      Right name -> nameToDeclaration name
      Left _ -> Nothing

  {-# INLINABLE identTraits #-}
  identTraits (WeederIdentifier (_, details)) =
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

  followWithSumInfo rf (WeederIdentifier (Right name, _)) =
    let evidenceInfos = maybe mempty Tree.flatten (getEvidenceTree rf name)
        instEvidenceInfos = evidenceInfos & filter \case
          EvidenceInfo _ _ _ (Just (EvInstBind _ _, ModuleScope, _)) -> True
          _ -> False
        evBindSiteDecls = mapMaybe (nameToDeclaration . evidenceVar) instEvidenceInfos
     in Set.fromList evBindSiteDecls
  followWithSumInfo _ _ = mempty

  {-# INLINABLE followable #-}
  followable i = ifFlagElse @fs @'AnalyseInstances
    (IsEvidenceUse `Set.member` identTraits i)
    False

  type WeederLocalInfo (HieWithFlags fs) = HieFile

  newtype WeederType (HieWithFlags fs) =
    WeederType HieTypeFix
    deriving Eq

  {-# INLINABLE lookupType #-}
  lookupType hf (WeederIdentifier (_, details )) =
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
