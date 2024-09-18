module Parse.Variable
  ( collectVarModule
  , mkVarR
  , mkVar
  ) where

import Types.Mod
import Language.Haskell.Exts
import qualified Data.HashSet as HS
import Data.List (intercalate)
import GHC.List (foldl')
import qualified Data.HashMap.Strict as HM
import Data.Maybe ( catMaybes, mapMaybe, fromMaybe )
import Control.Applicative ((<|>))
import Parse.Utils
import Data.HashMap.Strict ((!?), (!))
import GHC.Data.Maybe (isNothing)
import Parse.Type (findEntityDefForCons)
import Data.Foldable (find, Foldable (foldr'))
import Parse.ClassExt (findEntityDefForClassMethod)
import Types.Class (Merge((<:>)), NameLens (name_))

collectVarModule :: String -> [RepoModuleMap] -> Module Src -> ModuleT
collectVarModule repoName repoModules (Module _ (Just (ModuleHead _ (ModuleName _ modName) _ _)) _ _ decls) =
  let reposMap = foldl' (\ hm repo' -> HM.insert (_nameRepository' repo') (_modulesMap repo') hm) HM.empty repoModules
      moduleT = reposMap ! repoName ! modName
      payload = Payload
        { _repositoryPayload = repoName
        , _repoModules = repoModules
        , _reposMap = reposMap
        , _importsPayload = _importsModuleT moduleT
        , _modName = modName
        , _prefix = []
        , _localBindings = HS.empty
        }
      depsMap = foldl' (collectDepsDecl payload Nothing) HM.empty decls
      variables = map (updateVariableDeps repoName depsMap modName) $ _variables moduleT
      instances = map (updateInstanceDeps repoName depsMap modName) $ _instancesModuleT moduleT
  in moduleT
    { _variables = variables
    , _instancesModuleT = instances
    }
collectVarModule _ _ _other = error $ "Unknown module type " <> show _other

updateVariableDeps :: String -> Map DepsMapKey [Entity] -> String -> VarDesc -> VarDesc
updateVariableDeps repoName depsMap modName var' =
  let key = EntityD $ EntityDef repoName modName (_nameVarDesc var')
      mDeps = depsMap !? key
  in var' { _dependencies = fromMaybe (_dependencies var') mDeps }

updateInstanceDeps :: String -> Map DepsMapKey [Entity] -> String -> InstanceDesc -> InstanceDesc
updateInstanceDeps repoName depsMap modName instance' = instance'
  { _methodsInstanceDesc = map (updateInstanceMethod repoName depsMap modName (_headInstanceDesc instance')) (_methodsInstanceDesc instance') }

updateInstanceMethod :: String -> Map DepsMapKey [Entity] -> String -> String -> VarDesc -> VarDesc
updateInstanceMethod repoName depsMap modName instanceHead var' =
  let key = InstanceD $ InstanceDef repoName modName instanceHead
      mDeps = depsMap !? key
  in var' { _dependencies = fromMaybe (_dependencies var') mDeps }

collectDepsDecl :: Payload -> Maybe String -> Map DepsMapKey [Entity] -> Decl Src -> Map DepsMapKey [Entity]
collectDepsDecl payload mInstance depsMap (FunBind _ matches) =
  foldr (flip $ collectDepsMatch payload mInstance) depsMap matches
collectDepsDecl payload@Payload{..} mInstance depsMap decl@(PatBind _ (PVar _ name') _ mBinds) =
  let varN = intercalate "." $ _prefix <# getName name'
      key = maybe (EntityD $ EntityDef _repositoryPayload _modName varN) (InstanceD . InstanceDef _repositoryPayload _modName) mInstance
      deps = collectVarDecl payload decl $ fromMaybe [] (depsMap !? key)
      depsMap' = HM.insert key deps depsMap
  in maybe depsMap' (collectDepsBinds (payload { _prefix = _prefix <# varN }) depsMap') mBinds
collectDepsDecl payload _ depsMap (InstDecl _ _ instRule (Just instanceDecls)) =
  foldr (flip $ collectDepsInstDecl payload instRule) depsMap instanceDecls
collectDepsDecl _ _ depsMap _ = depsMap

collectDepsInstDecl :: Payload -> InstRule Src -> Map DepsMapKey [Entity] -> InstDecl Src -> Map DepsMapKey [Entity]
collectDepsInstDecl payload instRule depsMap (InsDecl _ decl) = collectDepsDecl payload (Just $ prettyPrint instRule) depsMap decl
collectDepsInstDecl _ _ depsMap _ = depsMap

collectDepsMatch :: Payload -> Maybe String -> Map DepsMapKey [Entity] -> Match Src -> Map DepsMapKey [Entity]
collectDepsMatch payload@Payload {..} mInstance depsMap match@(Match _ name' pats _ mBinds) =
  let varN = intercalate "." $ _prefix <# getName name'
      key = maybe (EntityD $ EntityDef _repositoryPayload _modName varN) (InstanceD . InstanceDef _repositoryPayload _modName) mInstance
      deps = flip (foldr' $ collectVarPat payload) pats $ collectVarMatch payload match $ fromMaybe [] (depsMap !? key)
      depsMap' = HM.insert key deps depsMap
  in maybe depsMap' (collectDepsBinds (payload { _prefix = _prefix <# varN }) depsMap') mBinds
collectDepsMatch payload@Payload {..} mInstance depsMap match@(InfixMatch _ _ name' pats _ mBinds) =
  let varN = intercalate "." $ _prefix <# getName name'
      key = maybe (EntityD $ EntityDef _repositoryPayload _modName varN) (InstanceD . InstanceDef _repositoryPayload _modName) mInstance
      deps = flip (foldr' $ collectVarPat payload) pats $ collectVarMatch payload match $ fromMaybe [] (depsMap !? key)
      depsMap' = HM.insert key deps depsMap
  in maybe depsMap' (collectDepsBinds (payload { _prefix = _prefix <# varN }) depsMap') mBinds

collectDepsBinds :: Payload -> Map DepsMapKey [Entity] -> Binds Src -> Map DepsMapKey [Entity]
collectDepsBinds payload depsMap (BDecls _ decls) = foldr (flip $ collectDepsDecl payload Nothing) depsMap decls
collectDepsBinds _ depsMap (IPBinds _ _) = depsMap

mkVarR :: [String] -> Decl Src -> [VarDesc] -> [VarDesc]
mkVarR prefix decl res =
  let res' = scanForBinds prefix decl res
  in maybe res' (<:> res') $ mkVar prefix decl

mkVar :: [String] -> Decl Src -> Maybe VarDesc
mkVar prefix decl@(FunBind srcInfo (firstPat:_)) =
  let desc = VarDesc
        { _nameVarDesc= intercalate "." $ prefix <# getNameFromPat firstPat
        , _locationVarDesc = mkRange srcInfo
        , _code = prettyPrint $ rmBinds decl
        , _dependencies = []
        }
  in Just desc
mkVar prefix decl@(TypeSig srcInfo (name':_) _) =
  let desc = VarDesc
        { _nameVarDesc = intercalate "." $ prefix <# getName name'
        , _locationVarDesc = mkRange srcInfo
        , _code = prettyPrint decl
        , _dependencies = []
        }
  in Just desc
mkVar prefix decl@(PatBind srcInfo (PVar _ name') _ _) =
  let desc = VarDesc
        { _nameVarDesc = intercalate "." $ prefix <# getName name'
        , _locationVarDesc = mkRange srcInfo
        , _code = prettyPrint $ rmBinds decl
        , _dependencies = []
        }
  in Just desc
mkVar _ _ = Nothing

rmBinds :: Decl Src -> Decl Src
rmBinds (FunBind srcInfo matches) = FunBind srcInfo $ map rmBindsMatches matches
rmBinds (PatBind srcInfo name' rhs _) = PatBind srcInfo name' rhs Nothing
rmBinds decl = decl

rmBindsMatches :: Match Src -> Match Src
rmBindsMatches (Match srcInfo name' pats rhs _) = Match srcInfo name' pats rhs Nothing
rmBindsMatches (InfixMatch srcInfo pat name' pats rhs _) = InfixMatch srcInfo pat name' pats rhs Nothing

scanForBinds :: [String] -> Decl Src -> [VarDesc] -> [VarDesc]
scanForBinds prefix (FunBind _ matches) res = foldr (scanMatch prefix) res matches
scanForBinds prefix (PatBind _ (PVar _ name') _ (Just binds')) res = scanBinds (prefix <# getName name') binds' res
scanForBinds _ _ res = res

scanMatch :: [String] -> Match Src -> [VarDesc] -> [VarDesc]
scanMatch prefix (Match _ name' _ _ (Just binds')) res = scanBinds (prefix <# getName name') binds' res
scanMatch prefix (InfixMatch _ _ name' _ _ (Just binds')) res = scanBinds (prefix <# getName name') binds' res
scanMatch _ _ res = res

scanBinds :: [String] -> Binds Src -> [VarDesc] -> [VarDesc]
scanBinds prefix (BDecls _ decls) res = foldr (mkVarR prefix) res decls
scanBinds _ (IPBinds _ _) res = res

collectVarDecl :: Payload -> Decl Src -> [Entity] -> [Entity]
collectVarDecl payload (FunBind _ matches) res = foldr (collectVarMatch payload) res matches
collectVarDecl payload (PatBind _ (PVar _ name') rhs _) res =
  let prefix = _prefix payload <# getName name'
      payload' = payload { _prefix = prefix }
  in collectVarRhs payload' rhs res
collectVarDecl _ _ res = res

collectVarMatch :: Payload -> Match Src -> [Entity] -> [Entity]
collectVarMatch payload (Match _ name' pats rhs _) res =
  let prefix = _prefix payload <# getName name'
      localBinds = foldl' (flip $ collectLocalBindsPat payload) (_localBindings payload) pats
      payload' = payload { _prefix = prefix, _localBindings = localBinds }
  in collectVarRhs payload' rhs res
collectVarMatch payload (InfixMatch _ pat name' pats rhs _) res =
  let prefix = _prefix payload <# getName name'
      localBinds = foldl' (flip $ collectLocalBindsPat payload) (_localBindings payload) (pat : pats)
      payload' = payload { _prefix = prefix, _localBindings = localBinds }
  in collectVarRhs payload' rhs res

collectVarBinds :: Payload -> Binds Src -> [Entity] -> [Entity]
collectVarBinds payload (BDecls _ decls) res = foldr (collectVarDecl payload) res decls
collectVarBinds _ (IPBinds _ _) res = res

collectVarQOp :: Payload -> QOp Src -> [Entity] -> [Entity]
collectVarQOp payload (QVarOp _ qName) res = maybe res (<:res) $ findEntityDefForVar payload qName <|> findEntityDefForClassMethod payload qName
collectVarQOp _ (QConOp _ _) res = res

collectVarPat :: Payload -> Pat Src -> [Entity] -> [Entity]
collectVarPat payload (PInfixApp _ pat1 _ pat2) res = collectVarPat payload pat1 $ collectVarPat payload pat2 res
collectVarPat payload (PApp _ _ pats) res = foldr' (collectVarPat payload) res pats
collectVarPat payload (PTuple _ _ pats) res = foldr' (collectVarPat payload) res pats
collectVarPat payload (PUnboxedSum _ _ _ pat) res = collectVarPat payload pat res
collectVarPat payload (PList _ pats) res = foldr' (collectVarPat payload) res pats
collectVarPat payload (PParen _ pat) res = collectVarPat payload pat res
collectVarPat payload (PRec _ _ patFields) res = foldr' (collectVarPatField payload) res patFields
collectVarPat payload (PAsPat _ _ pat) res = collectVarPat payload pat res
collectVarPat payload (PIrrPat _ pat) res = collectVarPat payload pat res
collectVarPat payload (PatTypeSig _ pat _) res = collectVarPat payload pat res
collectVarPat payload (PViewPat _ exp' pat) res = collectVarExp payload exp' $ collectVarPat payload pat res
collectVarPat payload (PBangPat _ pat) res = collectVarPat payload pat res
collectVarPat _ _ res = res

collectVarPatField :: Payload -> PatField Src -> [Entity] -> [Entity]
collectVarPatField payload (PFieldPat _ _ pat) res = collectVarPat payload pat res
collectVarPatField _ _ res = res

collectVarExp :: Payload -> Exp Src -> [Entity] -> [Entity]
collectVarExp payload (Var _ qName) res = maybe res (<:res) $ findEntityDefForVar payload qName <|> findEntityDefForClassMethod payload qName
collectVarExp payload (InfixApp _ exp1 opr exp2) res = collectVarExp payload exp1 $ collectVarQOp payload opr $ collectVarExp payload exp2 res
collectVarExp payload (App _ exp1 exp2) res = collectVarExp payload exp1 $ collectVarExp payload exp2 res
collectVarExp payload (NegApp _ exp') res = collectVarExp payload exp' res
collectVarExp payload (Lambda _ pats exp') res =
  let updatedLocalBinds = foldl' (flip $ collectLocalBindsPat payload) (_localBindings payload) pats
      payload' = payload { _localBindings = updatedLocalBinds }
  in flip (foldr' $ collectVarPat payload) pats $ collectVarExp payload' exp' res
collectVarExp payload (Let _ binds' exp') res =
  let updatedLocalBinds = collectLocalBinds payload binds' (_localBindings payload)
      payload' = payload { _localBindings = updatedLocalBinds }
  in collectVarExp payload' exp' $ collectVarBinds payload' binds' res
collectVarExp payload (If _ exp1 exp2 exp3) res = collectVarExp payload exp1 $ collectVarExp payload exp2 $ collectVarExp payload exp3 res
collectVarExp payload (MultiIf _ guardedRhs) res = foldr (collectVarGuardedRhs payload) res guardedRhs
collectVarExp payload (Case _ exp' alts) res = collectVarExp payload exp' $ foldr (collectVarAlt payload) res alts
collectVarExp payload (Do _ stmts) res = collectVarStmts payload stmts res
collectVarExp payload (MDo _ stmts) res = collectVarStmts payload stmts res
collectVarExp payload (Tuple _ _ exps) res = foldr (collectVarExp payload) res exps
collectVarExp payload (UnboxedSum _ _ _ exp') res = collectVarExp payload exp' res
collectVarExp payload (TupleSection _ _ mExps) res = foldr (collectVarExp payload) res $ catMaybes mExps
collectVarExp payload (List _ exps) res = foldr (collectVarExp payload) res exps
collectVarExp payload (ParArray _ exps) res = foldr (collectVarExp payload) res exps
collectVarExp payload (Paren _ exp') res = collectVarExp payload exp' res
collectVarExp payload (LeftSection _ exp' opr) res = collectVarExp payload exp' $ collectVarQOp payload opr res
collectVarExp payload (RightSection _ opr exp') res = collectVarQOp payload opr $ collectVarExp payload exp' res
collectVarExp payload (RecConstr _ _ fieldUpdates) res = foldr (collectVarFieldUpdate payload) res fieldUpdates
collectVarExp payload (RecUpdate _ exp' fieldUpdates) res = collectVarExp payload exp' $ foldr (collectVarFieldUpdate payload) res fieldUpdates
collectVarExp payload (EnumFrom _ exp') res = collectVarExp payload exp' res
collectVarExp payload (EnumFromTo _ exp1 exp2) res = collectVarExp payload exp1 $ collectVarExp payload exp2 res
collectVarExp payload (EnumFromThen _ exp1 exp2) res = collectVarExp payload exp1 $ collectVarExp payload exp2 res
collectVarExp payload (EnumFromThenTo _ exp1 exp2 exp3) res = collectVarExp payload exp1 $ collectVarExp payload exp2 $ collectVarExp payload exp3 res
collectVarExp payload (ParArrayFromTo _ exp1 exp2) res = collectVarExp payload exp1 $ collectVarExp payload exp2 res
collectVarExp payload (ParArrayFromThenTo _ exp1 exp2 exp3) res = collectVarExp payload exp1 $ collectVarExp payload exp2 $ collectVarExp payload exp3 res
collectVarExp payload (ListComp _ exp' qualStmts) res =
  let updatedLocalBinds = foldl' (flip $ collectLocalBindsQualStmt payload) (_localBindings payload) qualStmts
      payload' = payload { _localBindings = updatedLocalBinds }
  in collectVarExp payload' exp' $ foldr (collectVarQualStmt payload') res qualStmts
collectVarExp payload (ParComp _ exp' qualStmts) res =
  let updatedLocalBinds = foldl' (flip $ collectLocalBindsQualStmt payload) (_localBindings payload) (concat qualStmts)
      payload' = payload { _localBindings = updatedLocalBinds }
  in collectVarExp payload' exp' $ foldr (collectVarQualStmt payload') res (concat qualStmts)
collectVarExp payload (ParArrayComp _ exp' qualStmts) res =
  let updatedLocalBinds = foldl' (flip $ collectLocalBindsQualStmt payload) (_localBindings payload) (concat qualStmts)
      payload' = payload { _localBindings = updatedLocalBinds }
  in collectVarExp payload' exp' $ foldr (collectVarQualStmt payload') res (concat qualStmts)
collectVarExp payload (ExpTypeSig _ exp' _) res = collectVarExp payload exp' res
collectVarExp payload (Proc _ pat exp') res =
  let updatedLocalBinds = collectLocalBindsPat payload pat (_localBindings payload)
      payload' = payload { _localBindings = updatedLocalBinds }
  in collectVarPat payload pat $ collectVarExp payload' exp' res
collectVarExp payload (LeftArrApp _ exp1 exp2) res = collectVarExp payload exp1 $ collectVarExp payload exp2 res
collectVarExp payload (RightArrApp _ exp1 exp2) res = collectVarExp payload exp1 $ collectVarExp payload exp2 res
collectVarExp payload (LeftArrHighApp _ exp1 exp2) res = collectVarExp payload exp1 $ collectVarExp payload exp2 res
collectVarExp payload (RightArrHighApp _ exp1 exp2) res = collectVarExp payload exp1 $ collectVarExp payload exp2 res
collectVarExp payload (ArrOp _ exp') res = collectVarExp payload exp' res
collectVarExp payload (LCase _ alts) res = foldr (collectVarAlt payload) res alts
collectVarExp _ _ res = res

collectVarFieldUpdate :: Payload -> FieldUpdate Src -> [Entity] -> [Entity]
collectVarFieldUpdate payload (FieldUpdate _ _ exp') res = collectVarExp payload exp' res
collectVarFieldUpdate _ _ res = res

collectLocalBindsQualStmt :: Payload -> QualStmt Src -> HS.HashSet String -> HS.HashSet String
collectLocalBindsQualStmt payload (QualStmt _ (Generator _ pat _)) localBinds = collectLocalBindsPat payload pat localBinds
collectLocalBindsQualStmt _ _ localBinds = localBinds

collectVarQualStmt :: Payload -> QualStmt Src -> [Entity] -> [Entity]
collectVarQualStmt payload (QualStmt _ stmt) res = collectVarStmts payload [stmt] res
collectVarQualStmt payload (ThenTrans _ exp') res = collectVarExp payload exp' res
collectVarQualStmt payload (ThenBy _ exp1 exp2) res = collectVarExp payload exp1 $ collectVarExp payload exp2 res
collectVarQualStmt payload (GroupBy _ exp') res = collectVarExp payload exp' res
collectVarQualStmt payload (GroupUsing _ exp') res = collectVarExp payload exp' res
collectVarQualStmt payload (GroupByUsing _ exp1 exp2) res = collectVarExp payload exp1 $ collectVarExp payload exp2 res

collectVarStmts :: Payload -> [Stmt Src] -> [Entity] -> [Entity]
collectVarStmts _ [] res = res
collectVarStmts payload (Generator _ pat exp' : rem') res =
  let localBinds' = collectLocalBindsPat payload pat (_localBindings payload)
      payload' = payload { _localBindings = localBinds' }
  in collectVarExp payload exp' $ collectVarStmts payload' rem' res
collectVarStmts payload (Qualifier _ exp' : rem') res = collectVarExp payload exp' $ collectVarStmts payload rem' res
collectVarStmts payload (LetStmt _ binds' : rem') res =
  let updatedLocalBinds = collectLocalBinds payload binds' (_localBindings payload)
      payload' = payload { _localBindings = updatedLocalBinds }
  in collectVarBinds payload' binds' $ collectVarStmts payload' rem' res
collectVarStmts payload (RecStmt _ stmts : rem') res = collectVarStmts payload stmts $ collectVarStmts payload rem' res

collectVarAlt :: Payload -> Alt Src -> [Entity] -> [Entity]
collectVarAlt payload (Alt _ pat rhs _) res =
  let updatedLocalBinds = collectLocalBindsPat payload pat (_localBindings payload)
      payload' = payload { _localBindings = updatedLocalBinds }
  in collectVarPat payload pat $ collectVarRhs payload' rhs res

collectVarRhs :: Payload -> Rhs Src -> [Entity] -> [Entity]
collectVarRhs payload (UnGuardedRhs _ exp') res = collectVarExp payload exp' res
collectVarRhs payload (GuardedRhss _ guardedRhss) res = foldr (collectVarGuardedRhs payload) res guardedRhss

collectVarGuardedRhs :: Payload -> GuardedRhs Src -> [Entity] -> [Entity]
collectVarGuardedRhs payload (GuardedRhs _ stmts exp') res = collectVarExp payload exp' $ foldr (collectVarGuardedRhsStmt payload) res stmts

collectVarGuardedRhsStmt :: Payload -> Stmt Src -> [Entity] -> [Entity]
collectVarGuardedRhsStmt payload (Qualifier _ exp') res = collectVarExp payload exp' res
collectVarGuardedRhsStmt _ _ res = res

collectLocalBindsPat :: Payload -> Pat Src -> HS.HashSet String -> HS.HashSet String
collectLocalBindsPat _ (PVar _ name') localBinds = HS.insert (getName name') localBinds
collectLocalBindsPat payload (PInfixApp _ pat1 _ pat2) localBinds = collectLocalBindsPat payload pat1 $ collectLocalBindsPat payload pat2 localBinds
collectLocalBindsPat payload (PApp _ _ pats) localBinds = foldl' (flip $ collectLocalBindsPat payload) localBinds pats
collectLocalBindsPat payload (PTuple _ _ pats) localBinds = foldl' (flip $ collectLocalBindsPat payload) localBinds pats
collectLocalBindsPat payload (PUnboxedSum _ _ _ pat) localBinds = collectLocalBindsPat payload pat localBinds
collectLocalBindsPat payload (PList _ pats) localBinds = foldl' (flip $ collectLocalBindsPat payload) localBinds pats
collectLocalBindsPat payload (PParen _ pat) localBinds = collectLocalBindsPat payload pat localBinds
collectLocalBindsPat payload (PRec _ qName patFields) localBinds = foldl' (flip $ collectLocalBindsPatField payload qName) localBinds patFields
collectLocalBindsPat payload (PAsPat _ name' pat) localBinds = collectLocalBindsPat payload pat $ HS.insert (getName name') localBinds
collectLocalBindsPat payload (PatTypeSig _ pat _) localBinds = collectLocalBindsPat payload pat localBinds
collectLocalBindsPat payload (PViewPat _ _ pat) localBinds = collectLocalBindsPat payload pat localBinds
collectLocalBindsPat payload (PBangPat _ pat) localBinds = collectLocalBindsPat payload pat localBinds
collectLocalBindsPat _ _ localBinds = localBinds

collectLocalBindsPatField :: Payload -> QName Src -> PatField Src -> HS.HashSet String -> HS.HashSet String
collectLocalBindsPatField payload _ (PFieldPat _ _ pat) localBinds = collectLocalBindsPat payload pat localBinds
collectLocalBindsPatField _ _ (PFieldPun _ qName) localBinds = collectLocalBindsQName qName localBinds
collectLocalBindsPatField payload@Payload {..} consQ (PFieldWildcard _) localBinds =
  let mFields = findEntityDefForCons payload consQ >>=
        \ def -> find ((_nameEntityDef def ==) . name_) (_types (_reposMap ! _repositoryEntityDef def ! _moduleEntityDef def)) >>=
        getFieldsFromType (getNameQ consQ)
  in maybe localBinds (foldl' (flip HS.insert) localBinds) mFields

getFieldsFromType :: String -> TypeDesc -> Maybe [String]
getFieldsFromType conN (DataT dataT) = find ((conN ==) . name_) (_constructorsDataDesc dataT) >>= getFieldsFromCons
getFieldsFromType _ (TypeSynT _) = Nothing
getFieldsFromType conN (GadtT gadtT) = find ((conN ==) . name_) (_constructorsGadtDesc gadtT) >>= getFieldsFromCons

getFieldsFromCons :: ConsDesc -> Maybe [String]
getFieldsFromCons (OrdinaryCon _) = Nothing
getFieldsFromCons (InfixCon _) = Nothing
getFieldsFromCons (RecordCon _ fields) = Just fields

collectLocalBindsQName ::QName Src -> HS.HashSet String -> HS.HashSet String
collectLocalBindsQName (Qual _ _ name') localBinds = HS.insert (getName name') localBinds
collectLocalBindsQName (UnQual _ name') localBinds = HS.insert (getName name') localBinds
collectLocalBindsQName _ localBinds = localBinds

collectLocalBinds :: Payload -> Binds Src -> HS.HashSet String -> HS.HashSet String
collectLocalBinds payload (BDecls _ decls) localBinds = foldl' (flip $ collectLocalBindsDecl payload) localBinds decls
collectLocalBinds _ _ localBinds = localBinds

collectLocalBindsDecl :: Payload -> Decl Src -> HS.HashSet String -> HS.HashSet String
collectLocalBindsDecl payload (PatBind _ pat _ _) localBinds = collectLocalBindsPat payload pat localBinds
collectLocalBindsDecl _ (FunBind _ matches) localBinds = foldl' (flip collectLocalBindsMatch) localBinds matches
collectLocalBindsDecl _ _ localBinds = localBinds

collectLocalBindsMatch :: Match Src -> HS.HashSet String -> HS.HashSet String
collectLocalBindsMatch (Match _ name' _ _ _) localBinds = HS.insert (getName name') localBinds
collectLocalBindsMatch (InfixMatch _ _ name' _ _ _) localBinds = HS.insert (getName name') localBinds

findEntityDefForVar :: Payload -> QName SrcSpanInfo -> Maybe Entity
findEntityDefForVar payload@Payload {..} (Qual _ (ModuleName _ alias') vNameT) =
  let vName = getName vNameT
      moduleM = headMaybe $ mapMaybe (\ Import {_moduleImport} -> find (HM.member _moduleImport . _modulesMap) _repoModules
        >>= \ repo' -> lookForVar payload vName (_nameRepository' repo') (_modulesMap repo' ! _moduleImport))
        (filter (checkImportForVar (Just alias') vName) _importsPayload)
  in mkEntityDef vName <$> moduleM
findEntityDefForVar payload@Payload {..} (UnQual _ vNameT) =
  let vName = getName vNameT
      moduleM = headMaybe $ mapMaybe (\ Import {_moduleImport} -> find (HM.member _moduleImport . _modulesMap) _repoModules
        >>= \ repo' -> lookForVar payload vName (_nameRepository' repo') (_modulesMap repo' ! _moduleImport))
        (filter (checkImportForVar Nothing vName) _importsPayload)
  in if not (HS.member vName _localBindings)
    then checkInSelfMod payload vName <|> mkEntityDef vName <$> moduleM
    else Nothing
findEntityDefForVar _ (Special _ _) = Nothing

checkInSelfMod :: Payload -> String -> Maybe Entity
checkInSelfMod payload@Payload {..} varN = checkInSelfModAux _prefix []
  <|> (if null _prefix then mkEntityDef varN <$> lookForVar payload varN _repositoryPayload moduleT else Nothing)
  where
  checkInSelfModAux :: [String] -> [String] -> Maybe Entity
  checkInSelfModAux [] curPrefix =
    let varN' = intercalate "." $ curPrefix <# varN
    in if checkForVar True varN' moduleT then Just (mkEntityDef varN' (_repositoryPayload, moduleT)) else Nothing
  checkInSelfModAux (r:remPrefix) curPrefix =
    let varN' = intercalate "." $ curPrefix <# varN
    in checkInSelfModAux remPrefix (curPrefix <# r) <|> if checkForVar True varN' moduleT then Just (mkEntityDef varN' (_repositoryPayload, moduleT)) else Nothing

  moduleT :: ModuleT
  moduleT = _reposMap ! _repositoryPayload ! _modName

mkEntityDef :: String -> (String, ModuleT) -> Entity
mkEntityDef vName (repoName, moduleT) = Variable $ EntityDef repoName (_nameModuleT moduleT) vName

checkImportForVar :: Maybe String -> String -> Import -> Bool
checkImportForVar mAlias varN Import {..} = maybe (not _qualified) ((_alias ==) . Just) mAlias && matchSpecsForVar _specsList varN

matchSpecsForVar :: SpecsList -> String -> Bool
matchSpecsForVar (Include list) varN = any (matchImportItemVar varN) list
matchSpecsForVar (Hide list) varN = not $ any (matchImportItemVar varN) list

matchImportItemVar :: String -> ImportItem -> Bool
matchImportItemVar varN (VarImportItem name') = name' == varN
matchImportItemVar _ _ = False

lookForVar :: Payload -> String -> String -> ModuleT -> Maybe (String, ModuleT)
lookForVar payload@Payload {..} varN repoName moduleT =
  let modWithRepo = if checkForVar False varN moduleT
        then Just (repoName, _nameModuleT moduleT)
        else lookForVarInExport payload moduleT varN (_exports moduleT)
  in modWithRepo >>= \ (repoName', modName') -> return (repoName', _reposMap ! repoName' ! modName')

lookForVarInExport :: Payload -> ModuleT -> String -> ExportList -> Maybe (String, String)
lookForVarInExport _ _ _ AllE = Nothing
lookForVarInExport payload moduleT typeN (SomeE expList) = headMaybe . mapMaybe (lookForVarInExport' payload moduleT typeN) $ expList

lookForVarInExport' :: Payload -> ModuleT -> String -> ExportItem -> Maybe (String, String)
lookForVarInExport' payload@Payload {..} moduleT varN ExportVar {..}
  | _nameExportItem == varN =
    let imports' = filter (checkImportForVar _qualifier varN) (_importsModuleT moduleT)
    in headMaybe $ mapMaybe (\ Import {_moduleImport} -> find (HM.member _moduleImport . _modulesMap) _repoModules
      >>= \ repo' -> fmap _nameModuleT <$> lookForVar payload varN (_nameRepository' repo') (_modulesMap repo' ! _moduleImport)) imports'
  | otherwise = Nothing
lookForVarInExport' payload@Payload {..} moduleT varN ExportModule {..}
  | _nameExportItem /= _nameModuleT moduleT =
    case find (HM.member _nameExportItem . _modulesMap) _repoModules
      >>= \ RepoModuleMap {..} -> return (_nameRepository', _modulesMap ! _nameExportItem) of
      Just (repoName, moduleT') -> fmap _nameModuleT <$> lookForVar payload varN repoName moduleT'
      Nothing ->
        let imports' = filter (checkImportForVar (Just _nameExportItem) varN) (_importsModuleT moduleT)
        in headMaybe $ mapMaybe (\ Import {_moduleImport} -> find (HM.member _moduleImport . _modulesMap) _repoModules
          >>= \ repo' -> fmap _nameModuleT <$> lookForVar payload varN (_nameRepository' repo') (_modulesMap repo' ! _moduleImport)) imports'
  | otherwise = Nothing
lookForVarInExport' _ _ _ ExportType {} = Nothing

checkForVar :: Bool -> String -> ModuleT -> Bool
checkForVar selfMod varN moduleT = (selfMod || checkExportListForVar (_exports moduleT) varN) && HS.member varN (_variablesSet moduleT)

checkExportListForVar :: ExportList -> String -> Bool
checkExportListForVar AllE _ = True
checkExportListForVar (SomeE items) typeN = any (matchExportItemForVar typeN) items

matchExportItemForVar :: String -> ExportItem -> Bool
matchExportItemForVar varN (ExportVar {..}) = isNothing _qualifier && _nameExportItem == varN
matchExportItemForVar _ _ = False
