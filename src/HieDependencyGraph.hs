{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module HieDependencyGraph (analyseHieFiles) where

import GHC.Iface.Ext.Types
import qualified Algebra.Graph.Labelled as G
import Algebra.Graph.Labelled ((>-), (-<))
import qualified Data.Map.Strict as Map
import GHC.Iface.Ext.Utils
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Data.Set (Set)
import Data.Sequence (Seq)
import GHC.Types.Name
import GHC.Unit.Types
import Data.Foldable
import qualified Data.Set as Set
import Control.Monad
import Data.Maybe
import GHC.Iface.Type
import GHC.Plugins (showSDocOneLine, defaultSDocContext)
import Control.Monad.Trans.Class


data Declaration = Declaration
  { declModule :: Module
  , declOccName :: OccName
  } deriving (Eq, Ord)

data Dependency = TypeOf
  deriving (Eq, Ord)

-- | hasEdge x y <=> x depends on y
type DepGraph = G.Graph (Set Dependency) Declaration


analyseHieFiles :: [HieFile] -> DepGraph
analyseHieFiles hieFiles =
  let asts = concatMap (Map.elems . getAsts . hie_asts) hieFiles
      rf = generateReferencesMap asts
      graph = mconcat $ map ((`execState` mempty) . analyseHieFile) hieFiles
      graph' = analyseEvidenceUses rf graph
   in graph'


analyseEvidenceUses :: RefMap a -> DepGraph -> DepGraph
analyseEvidenceUses rf g = undefined


analyseHieFile :: HieFile -> State DepGraph ()
analyseHieFile hf = flip runReaderT hf $ do
  lift $ modify' (G.overlay $ initialGraph hf)
  undefined


-- | Takes every name in the AST, and initialises a graph with
-- all type names they depend on.
initialGraph :: HieFile -> DepGraph
initialGraph hf@HieFile{ hie_asts = HieASTs hieAsts } = do
  let asts = Map.elems hieAsts
      decls = concatMap (toList . findIdentifiers' (const True)) asts
  G.edges $ do
    (d, IdentifierDetails{identType}, _) <- decls
    t <- maybe mzero pure identType
    let names = Set.toList . typeToNames $ lookupType hf t
        typeDecls = mapMaybe nameToDeclaration names
    typeDecl <- typeDecls
    pure (Set.singleton TypeOf, d, typeDecl)


lookupType :: HieFile -> TypeIndex -> HieTypeFix
lookupType hf t = recoverFullType t $ hie_types hf


displayType :: HieTypeFix -> String
displayType = showSDocOneLine defaultSDocContext . pprIfaceSigmaType ShowForAllWhen . hieTypeToIface


-- | Names mentioned within the type.
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


findIdentifiers
  :: ( Set ContextInfo -> Bool )
  -> HieAST a
  -> Seq Declaration
findIdentifiers f = fmap (\(d, _, _) -> d) . findIdentifiers' f


findIdentifiers'
  :: ( Set ContextInfo -> Bool )
  -> HieAST a
  -> Seq (Declaration, IdentifierDetails a, HieAST a)
findIdentifiers' f n@Node{ sourcedNodeInfo, nodeChildren } =
     foldMap
       (\case
           ( Left _, _ ) ->
             mempty

           ( Right name, ids@IdentifierDetails{ identInfo } ) ->
             if f identInfo then
               (, ids, n) <$> foldMap pure (nameToDeclaration name)

             else
               mempty
           )
       (foldMap (Map.toList . nodeIdentifiers) (getSourcedNodeInfo sourcedNodeInfo))
  <> foldMap ( findIdentifiers' f ) nodeChildren


nameToDeclaration :: Name -> Maybe Declaration
nameToDeclaration name = do
  m <- nameModule_maybe name
  return Declaration { declModule = m, declOccName = nameOccName name }
