{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language ViewPatterns #-}
{-# language OverloadedStrings #-}

module Weeder
  ( -- * Analysis
    Analysis(..)
  , analyseEvidenceUses
  , analyseHieFile
  , emptyAnalysis
  , outputableDeclarations

    -- ** Reachability
  , Root(..)
  , reachable

    -- * Declarations
  , Declaration(..)
  )
   where

-- algebraic-graphs
import Algebra.Graph ( Graph, edge, empty, overlay, vertex, stars, star, overlays )
import Algebra.Graph.ToGraph ( dfs )

-- base
import Control.Applicative ( Alternative, asum )
import Control.Monad ( guard, msum, when, unless, mzero, void, (>=>) )
import Data.Traversable ( for )
import Data.Maybe ( mapMaybe, fromMaybe, listToMaybe )
import Data.Foldable ( for_, traverse_, toList )
import Data.Function ( (&) )
import Data.List ( intercalate )
import GHC.Generics ( Generic )
import Prelude hiding ( span )

-- containers
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Sequence ( Seq )
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Tree ( Tree, rootLabel )
import qualified Data.Tree as Tree

-- generic-lens
import Data.Generics.Labels ()

-- ghc
import GHC.Data.FastString ( FastString )
import GHC.Types.Avail
  ( AvailInfo( Avail, AvailTC )
  , GreName( NormalGreName, FieldGreName )
  )
import GHC.Types.FieldLabel ( FieldLabel( FieldLabel, flSelector ) )
import GHC.Iface.Ext.Types
  ( BindType( RegularBind )
  , ContextInfo(.. )
  , DeclType( DataDec, ClassDec, ConDec, SynDec, FamDec )
  , EvVarSource ( EvInstBind, cls )
  , HieAST( Node, nodeChildren, sourcedNodeInfo )
  , HieASTs( HieASTs )
  , HieFile( HieFile, hie_asts, hie_exports, hie_module, hie_hs_file, hie_types )
  , HieType( HTyVarTy, HAppTy, HTyConApp, HForAllTy, HFunTy, HQualTy, HLitTy, HCastTy, HCoercionTy )
  , HieArgs( HieArgs )
  , HieTypeFix( Roll )
  , IdentifierDetails( IdentifierDetails, identInfo, identType )
  , NodeAnnotation( NodeAnnotation, nodeAnnotType )
  , NodeInfo( nodeIdentifiers )
  , Scope( ModuleScope )
  , RecFieldContext ( RecFieldOcc )
  , TypeIndex
  , getSourcedNodeInfo
  )
import GHC.Iface.Ext.Utils
  ( EvidenceInfo( EvidenceInfo, evidenceVar )
  , RefMap
  , getEvidenceTree
  , recoverFullType
  )
import GHC.Unit.Module ( Module, moduleStableString )
import GHC.Utils.Outputable ( defaultSDocContext, showSDocOneLine, ppr )
import GHC.Iface.Type
  ( IfaceTyCon (IfaceTyCon, ifaceTyConName)
  )
import GHC.Types.Name
  ( Name, nameModule_maybe, nameOccName
  , OccName
  , isDataOcc
  , isDataSymOcc
  , isTcOcc
  , isTvOcc
  , isVarOcc
  , occNameString
  )

-- lens
import Control.Lens ( (%=) )

-- mtl
import Control.Monad.State.Class ( MonadState )
import Control.Monad.Reader.Class ( MonadReader, asks )

-- parallel
import Control.Parallel.Strategies ( NFData )

-- transformers
import Control.Monad.Trans.Maybe ( runMaybeT, MaybeT )
import Control.Monad.Trans.Reader ( runReaderT )

-- weeder
import Weeder.Config ( Config, ConfigType( Config, typeClassRoots, unusedTypes ) )
import Weeder.Types
import Data.Either


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


-- | All information maintained by 'analyseHieFile'.
data Analysis =
  Analysis
    { dependencyGraph :: Graph Declaration
      -- ^ A graph between declarations, capturing dependencies.
    , declarationSites :: Map Declaration (Set Int)
      -- ^ A partial mapping between declarations and their line numbers.
      -- This Map is partial as we don't always know where a Declaration was
      -- defined (e.g., it may come from a package without source code).
      -- We capture a set of sites, because a declaration may be defined in
      -- multiple locations, e.g., a type signature for a function separate
      -- from its definition.
    , implicitRoots :: Set Root
      -- ^ Stores information on Declarations that may be automatically marked
      -- as always reachable. This is used, for example, to capture knowledge 
      -- not yet modelled in weeder, or to mark all instances of a class as 
      -- roots.
    , exports :: Map Module ( Set Declaration )
      -- ^ All exports for a given module.
    , modulePaths :: Map Module FilePath
      -- ^ A map from modules to the file path to the .hs file defining them.
    , prettyPrintedType :: Map Declaration String
      -- ^ Used to match against the types of instances and to replace the
      -- appearance of declarations in the output
    , requestedEvidence :: Map Declaration (Set Name)
      -- ^ Map from declarations to the names containing evidence uses that
      -- should be followed and treated as dependencies of the declaration.
      -- We use this to be able to delay analysing evidence uses until later,
      -- allowing us to begin the rest of the analysis before we have read all
      -- hie files.
    }
  deriving
    ( Generic, NFData )


instance Semigroup Analysis where
  (<>) (Analysis a1 b1 c1 d1 e1 f1 g1) (Analysis a2 b2 c2 d2 e2 f2 g2)= 
    Analysis (a1 `overlay` a2) (Map.unionWith (<>) b1 b2) (c1 <> c2) (Map.unionWith (<>) d1 d2) (e1 <> e2) (f1 <> f2) (Map.unionWith (<>) g1 g2)


instance Monoid Analysis where
  mempty = emptyAnalysis


data AnalysisInfo =
  AnalysisInfo
    { currentHieFile :: HieFile
    , weederConfig :: Config
    }


-- | The empty analysis - the result of analysing zero @.hie@ files.
emptyAnalysis :: Analysis
emptyAnalysis = Analysis empty mempty mempty mempty mempty mempty mempty


-- | A root for reachability analysis.
data Root
  = -- | A given declaration is a root.
    DeclarationRoot Declaration
  | -- | We store extra information for instances in order to be able
    -- to specify e.g. all instances of a class as roots.
    InstanceRoot 
      Declaration -- ^ Declaration of the instance
      Declaration -- ^ Declaration of the parent class
  | -- | All exported declarations in a module are roots.
    ModuleRoot Module
  deriving
    ( Eq, Ord, Generic, NFData )


-- | Determine the set of all declaration reachable from a set of roots.
reachable :: Analysis -> Set Root -> Set Declaration
reachable Analysis{ dependencyGraph, exports } roots =
  Set.fromList ( dfs dependencyGraph ( foldMap rootDeclarations roots ) )

  where

    rootDeclarations = \case
      DeclarationRoot d -> [ d ]
      InstanceRoot d _ -> [ d ] -- filter InstanceRoots in `Main.hs`
      ModuleRoot m -> foldMap Set.toList ( Map.lookup m exports )


-- | The set of all declarations that could possibly
-- appear in the output.
outputableDeclarations :: Analysis -> Set Declaration
outputableDeclarations Analysis{ declarationSites } =
  Map.keysSet declarationSites


-- Generate an initial graph of the current HieFile.
initialGraph :: AnalysisInfo -> Graph Declaration
initialGraph info =
  let hf@HieFile{ hie_asts = HieASTs hieAsts } = currentHieFile info
      Config{ unusedTypes } = weederConfig info
      asts = Map.elems hieAsts
      decls = concatMap (toList . findIdentifiers' (const True)) asts
  in if unusedTypes
    then stars do 
      (d, IdentifierDetails{identType}, _) <- decls
      t <- maybe mzero pure identType
      let ns = Set.toList $ typeToNames (lookupType hf t)
          ds = mapMaybe nameToDeclaration ns
      guard $ not (null ds)
      pure (d, ds)
    else mempty


-- | Incrementally update 'Analysis' with information in a 'HieFile'.
analyseHieFile :: (MonadState Analysis m) => Config -> HieFile -> m ()
analyseHieFile weederConfig hieFile =
  let info = AnalysisInfo hieFile weederConfig
   in runReaderT analyseHieFile' info


analyseHieFile' :: ( MonadState Analysis m, MonadReader AnalysisInfo m ) => m ()
analyseHieFile' = do
  HieFile{ hie_asts = HieASTs hieASTs, hie_exports, hie_module, hie_hs_file } <- asks currentHieFile
  #modulePaths %= Map.insert hie_module hie_hs_file
  
  g <- asks initialGraph
  #dependencyGraph %= overlay g

  for_ hieASTs analyseAST

  for_ hie_exports ( analyseExport hie_module )


lookupType :: HieFile -> TypeIndex -> HieTypeFix
lookupType hf t = recoverFullType t $ hie_types hf


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


analyseExport :: MonadState Analysis m => Module -> AvailInfo -> m ()
analyseExport m = \case
  Avail (NormalGreName name) ->
    traverse_ addExport $ nameToDeclaration name

  Avail (FieldGreName (FieldLabel{ flSelector })) ->
    traverse_ addExport $ nameToDeclaration flSelector

  AvailTC name pieces -> do
    for_ ( nameToDeclaration name ) addExport

    for_ pieces \case
      NormalGreName name' ->
        traverse_ addExport $ nameToDeclaration name'

      FieldGreName (FieldLabel{ flSelector }) ->
        traverse_ addExport $ nameToDeclaration flSelector

  where

    addExport :: MonadState Analysis m => Declaration -> m ()
    addExport d = #exports %= Map.insertWith (<>) m ( Set.singleton d )


-- | @addDependency x y@ adds the information that @x@ depends on @y@.
addDependency :: MonadState Analysis m => Declaration -> Declaration -> m ()
addDependency x y =
  #dependencyGraph %= overlay ( edge x y )


addImplicitRoot :: MonadState Analysis m => Declaration -> m ()
addImplicitRoot x =
  #implicitRoots %= Set.insert (DeclarationRoot x)


addInstanceRoot' :: (MonadState Analysis m, MonadReader AnalysisInfo m) => Declaration -> WeederType -> Declaration -> m ()
addInstanceRoot' x t cls = do
  #implicitRoots %= Set.insert (InstanceRoot x cls)

  -- since instances will not appear in the output if typeClassRoots is True
  Config{ typeClassRoots } <- asks weederConfig
  unless typeClassRoots $ do
    let str = showSDocOneLine defaultSDocContext $ ppr t
    #prettyPrintedType %= Map.insert x str


define :: MonadState Analysis m => Declaration -> WeederNode -> m ()
define decl WeederNode{ nodeLocation = Just l } = do
  #declarationSites %= Map.insertWith Set.union decl ( Set.singleton l )
  #dependencyGraph %= overlay ( vertex decl )
define _ _ = pure ()


analyseAST :: ( MonadState Analysis m, MonadReader AnalysisInfo m ) => HieAST TypeIndex -> m ()
analyseAST n = do
  Config{ unusedTypes } <- asks weederConfig
  hf <- asks currentHieFile
  let n' = searchContextInfo (toWeederAST hf n)
  topLevelAnalysis' n' \a -> msum . map ($ a) $
    [ analyseStandaloneDeriving
    , analyseInstanceDeclaration
    , analyseBinding
    , analyseRewriteRule
    , analyseClassDeclaration
    , analyseDataDeclaration
    , analysePatternSynonyms
    ] ++ if unusedTypes then
    [ analyseTypeSynonym
    , analyseFamilyDeclaration
    , analyseFamilyInstance
    , analyseTypeSignature
    ] else []


topLevelAnalysis' :: Monad m => Tree x -> (Tree x -> MaybeT m ()) -> m ()
topLevelAnalysis' n = void . runMaybeT . topLevelAnalysis n


annsContain :: WeederNode -> (FastString, FastString) -> Bool
annsContain WeederNode{ nodeAnns } ann =
  uncurry NodeAnnotation ann `elem` nodeAnns


lookupMonoid :: (Ord k, Monoid a) => k -> Map k a -> a
lookupMonoid k = fromMaybe mempty . Map.lookup k 


identDeclaration :: WeederIdentifier -> Maybe Declaration
identDeclaration = identName >=> nameToDeclaration


declsOfContext :: (ContextInfo -> Bool) -> ContextInfoMap -> [Declaration]
declsOfContext f m = Map.elems m' >>= mapMaybe (identDeclaration . snd)
  where
      m' = Map.filterWithKey (\k _ -> f k) m


analyseBinding :: (Alternative m, MonadState Analysis m, MonadReader AnalysisInfo m) => Tree (WeederNode, ContextInfoMap) -> m ()
analyseBinding (rootLabel -> (n, contextMap) ) = do
  let bindAnns = [("FunBind", "HsBindLR"), ("PatBind", "HsBindLR")]
      ds = declsOfContext isDeclaration contextMap
      uses' = declsOfContext isUse contextMap

  guard $ any (annsContain n) bindAnns

  for_ ds \d -> do
    define d n

    requestEvidence' contextMap d

    for_ uses' $ addDependency d


analyseRewriteRule :: ( Alternative m, MonadState Analysis m ) => Tree (WeederNode, ContextInfoMap) -> m ()
analyseRewriteRule (rootLabel -> (n, contextMap)) = do
  guard $ annsContain n ("HsRule", "RuleDecl")
  
  let uses' = declsOfContext isUse contextMap

  for_ uses' addImplicitRoot


extractEvInstBinds :: ContextInfoMap -> [(Declaration, Declaration, WeederIdentifier, Tree WeederNode)]
extractEvInstBinds contextMap = do
  (k, v) <- Map.toList contextMap
  c <- maybe mzero pure (getEvidenceBindClass k)
  (ast, i) <- v
  d <- maybe mzero pure (identDeclaration i)
  pure (d, c, i, ast)



analyseInstanceDeclaration :: ( Alternative m, MonadState Analysis m, MonadReader AnalysisInfo m ) => Tree (WeederNode, ContextInfoMap) -> m ()
analyseInstanceDeclaration (rootLabel -> (n, contextMap)) = do
  guard $ annsContain n ("ClsInstD", "InstDecl")

  let evInstBinds = extractEvInstBinds contextMap
      uses' = declsOfContext isUse contextMap

  for_ evInstBinds \(d, c, i, _) -> do
    -- This makes instance declarations show up in 
    -- the output if type-class-roots is set to False.
    define d n

    requestEvidence' contextMap d

    for_ uses' $ addDependency d

    case identWeederType i of
      Just t -> addInstanceRoot' d t c
      Nothing -> pure ()


analyseClassDeclaration :: ( Alternative m, MonadState Analysis m, MonadReader AnalysisInfo m ) => Tree (WeederNode, ContextInfoMap) -> m ()
analyseClassDeclaration (rootLabel -> (n, contextMap)) = do
  guard $ annsContain n ("ClassDecl", "TyClDecl")

  let ds = declsOfContext isClassDeclaration contextMap
      allDecls = Map.elems contextMap >>= mapMaybe (identDeclaration . snd)

  for_ ds $ \d -> do
    define d n

    requestEvidence' contextMap d

    for_ allDecls $ addDependency d
 

  where

    isClassDeclaration =
      \case
        Decl ClassDec _ ->
          True

        _ ->
          False


analyseDataDeclaration :: ( Alternative m, MonadState Analysis m, MonadReader AnalysisInfo m ) => Tree (WeederNode, ContextInfoMap) -> m ()
analyseDataDeclaration ast@(rootLabel -> (n, contextMap)) = do
  guard $ annsContain n ("DataDecl", "TyClDecl")

  Config{ unusedTypes } <- asks weederConfig

  (results, _) <-
    topLevelAnalysis ast \a -> asum
      [ fmap Left (getConstructor a)
      , fmap Right (getDerivingClause a)
      ]

  let (constructors, derivingClauses) = partitionEithers results
      dataDecs = declsOfContext isDataDec contextMap
      uses' = declsOfContext isUse contextMap

  for_ (listToMaybe dataDecs)
    \dataTypeName -> do
      when unusedTypes $
        define dataTypeName n

      for_ constructors \(rootLabel -> (_, conMap)) -> do
        let conDecs = declsOfContext isConDec conMap
        for ( listToMaybe conDecs ) (`addDependency` dataTypeName)

      -- This creates a mutual dependency between the data type and its
      -- constructors, but that doesn't matter since we do not analyse
      -- unused constructors
      for_ uses' (addDependency dataTypeName)

  for_ derivingClauses \(rootLabel -> (_, derivMap)) -> do
    let evInstBinds = extractEvInstBinds derivMap
    for_ evInstBinds \(d, c, i, instAst@(rootLabel -> instNode)) -> do
      -- May be better to make use of 'topLevelAnalysis' here to avoid
      -- the extra 'searchContextInfo' call
      let instMap = snd . rootLabel $ searchContextInfo instAst
          instUses = declsOfContext isUse instMap

      define d instNode

      requestEvidence' instMap d

      for_ instUses $ addDependency d

      case identWeederType i of
        Just t -> addInstanceRoot' d t c
        Nothing -> pure ()

  where

    isDataDec = \case
      Decl DataDec _ -> True
      _              -> False

    isConDec = \case
      Decl ConDec _ -> True
      _             -> False


getConstructor :: Alternative m => Tree (WeederNode, ContextInfoMap) -> m (Tree (WeederNode, ContextInfoMap))
getConstructor ast@(rootLabel -> (WeederNode{ nodeAnns }, _)) = do
  guard $ any ((== "ConDecl") . nodeAnnotType) nodeAnns

  pure ast


getDerivingClause :: Alternative m => Tree (WeederNode, ContextInfoMap) -> m (Tree (WeederNode, ContextInfoMap))
getDerivingClause ast@(rootLabel -> (WeederNode{ nodeAnns }, _)) = do
  guard $ any ((== "HsDerivingClause") . nodeAnnotType) nodeAnns

  pure ast


analyseStandaloneDeriving :: ( Alternative m, MonadState Analysis m, MonadReader AnalysisInfo m ) => Tree (WeederNode, ContextInfoMap) -> m ()
analyseStandaloneDeriving (rootLabel -> (n, contextMap)) = do
  guard $ annsContain n ("DerivDecl", "DerivDecl")

  let evInstBinds = extractEvInstBinds contextMap
      uses' = declsOfContext isUse contextMap

  for_ evInstBinds \(d, c, i, _) -> do
    define d n

    requestEvidence' contextMap d

    for_ uses' $ addDependency d

    case identWeederType i of
      Just t -> addInstanceRoot' d t c
      Nothing -> pure ()


analyseTypeSynonym :: ( Alternative m, MonadState Analysis m ) => Tree (WeederNode, ContextInfoMap) -> m ()
analyseTypeSynonym (rootLabel -> (n, contextMap)) = do
  guard $ annsContain n ("SynDecl", "TyClDecl")

  let ds = declsOfContext isTypeSynonym contextMap
      uses' = declsOfContext isUse contextMap

  for_ ds $ \d -> do
    define d n

    for_ uses' (addDependency d)

  where

    isTypeSynonym =
      \case
        Decl SynDec _ -> True
        _             -> False


analyseFamilyDeclaration :: ( Alternative m, MonadState Analysis m ) => Tree (WeederNode, ContextInfoMap) -> m ()
analyseFamilyDeclaration (rootLabel -> (n, contextMap)) = do
  guard $ annsContain n ("FamDecl", "TyClDecl")

  let ds = declsOfContext isFamDec contextMap
      uses' = declsOfContext isUse contextMap

  for_ ds $ \d -> do
    define d n

    for_ uses' (addDependency d)

  where

    isFamDec =
      \case
        Decl FamDec _ -> True
        _             -> False


analyseFamilyInstance :: ( Alternative m, MonadState Analysis m ) => Tree (WeederNode, ContextInfoMap) -> m ()
analyseFamilyInstance (rootLabel -> (n, contextMap)) = do
  guard $ annsContain n ("TyFamInstD", "InstDecl")

  for_ ( declsOfContext isUse contextMap ) addImplicitRoot


analyseTypeSignature :: ( Alternative m, MonadState Analysis m ) => Tree (WeederNode, ContextInfoMap) -> m ()
analyseTypeSignature (rootLabel -> (n, contextMap)) = do
  guard $ annsContain n ("TypeSig", "Sig")

  let ds = declsOfContext isTypeSigDecl contextMap
      uses' = declsOfContext isUse contextMap

  for_ ds $
    for_ uses' . addDependency

  where

    isTypeSigDecl =
      \case
        TyDecl -> True
        _      -> False


analysePatternSynonyms :: ( Alternative m, MonadState Analysis m ) => Tree (WeederNode, ContextInfoMap) -> m ()
analysePatternSynonyms (rootLabel -> (n, contextMap)) = do
  guard $ annsContain n ("PatSynBind", "HsBindLR")

  let ds = declsOfContext isDeclaration contextMap
      uses' = declsOfContext isUse contextMap

  for_ ds $ for_ uses' . addDependency


isDeclaration :: ContextInfo -> Bool
isDeclaration = \case
  -- Things that count as declarations
  ValBind RegularBind ModuleScope _ -> True
  PatternBind ModuleScope _ _       -> True
  Decl _ _                          -> True
  TyDecl                            -> True
  ClassTyDecl{}                     -> True

  -- Anything else is not a declaration
  _ -> False


-- | Version of findIdentifiers containing more information,
-- namely the IdentifierDetails of the declaration and the
-- node it was found in.
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


isUse :: ContextInfo -> Bool
isUse = \case
  Use -> True
  -- not RecFieldMatch and RecFieldDecl because they occur under
  -- data declarations, which we do not want to add as dependencies
  -- because that would make the graph no longer acyclic
  -- RecFieldAssign will be most likely accompanied by the constructor
  RecField RecFieldOcc _ -> True
  _ -> False


getEvidenceBindClass :: ContextInfo -> Maybe Declaration
getEvidenceBindClass (EvidenceVarBind a@EvInstBind{} ModuleScope _) =
  nameToDeclaration (cls a)
getEvidenceBindClass _ = Nothing


nameToDeclaration :: Name -> Maybe Declaration
nameToDeclaration name = do
  m <- nameModule_maybe name
  return Declaration { declModule = m, declOccName = nameOccName name }


-- | Add evidence uses found in the given 'ContextInfoMap' to 'requestedEvidence'
requestEvidence' :: ( MonadState Analysis m, MonadReader AnalysisInfo m ) => ContextInfoMap -> Declaration -> m ()
requestEvidence' contextMap d = do
  Config{ typeClassRoots } <- asks weederConfig

  let names = mapMaybe (identName . snd) $ lookupMonoid EvidenceVarUse contextMap
  
  -- If type-class-roots flag is set then we don't need to follow
  -- evidence uses as the binding sites will be roots anyway
  unless typeClassRoots $
    #requestedEvidence %= Map.insertWith (<>) d (Set.fromList names)


-- | Follow the given evidence uses back to their instance bindings,
-- and connect the declaration to those bindings.
followEvidenceUses :: RefMap TypeIndex -> Declaration -> Set Name -> Graph Declaration
followEvidenceUses refMap d names =
  let getEvidenceTrees = mapMaybe (getEvidenceTree refMap) . Set.toList
      evidenceInfos = concatMap Tree.flatten (getEvidenceTrees names)
      instanceEvidenceInfos = evidenceInfos & filter \case
        EvidenceInfo _ _ _ (Just (EvInstBind _ _, ModuleScope, _)) -> True
        _ -> False
      evBindSiteDecls = mapMaybe (nameToDeclaration . evidenceVar) instanceEvidenceInfos
   in star d evBindSiteDecls


-- | Follow evidence uses listed under 'requestedEvidence' back to their 
-- instance bindings, and connect their corresponding declaration to those bindings.
analyseEvidenceUses :: RefMap TypeIndex -> Analysis -> Analysis
analyseEvidenceUses rf a@Analysis{ requestedEvidence, dependencyGraph } =
  let graphs = map (uncurry (followEvidenceUses rf)) $ Map.toList requestedEvidence
   in a { dependencyGraph = overlays (dependencyGraph : graphs) }
