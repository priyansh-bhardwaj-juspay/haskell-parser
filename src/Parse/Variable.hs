module Parse.Variable
  ( collectVarModule
  , mkVarR
  , mkVar
  ) where

import Types
import Language.Haskell.Exts
import qualified Data.HashSet as HS
import Data.List (intercalate)
import GHC.List (foldl')
import Data.Generics.Labels ()
import Control.Lens hiding (List)
import qualified Data.HashMap.Strict as HM
import Data.Maybe ( catMaybes, mapMaybe, fromMaybe )
import Control.Applicative ((<|>))
import Parse.Utils
import Data.HashMap.Strict ((!?), (!))
import GHC.Data.Maybe (isNothing)
import Parse.Type (findEntityDefForCons)
import Data.Foldable (find, Foldable (foldr'))
import Parse.ClassExt (findEntityDefForClassMethod)

collectVarModule :: Map String ModuleT -> Module Src -> ModuleT
collectVarModule modulesMap (Module _ (Just (ModuleHead _ (ModuleName _ modName) _ _)) _ _ decls) =
  let moduleT = modulesMap ! modName
      payload = Payload
        { modulesMap = modulesMap
        , imports = moduleT ^. #imports
        , modName = modName
        , prefix = []
        , localBindings = HS.empty
        }
      depsMap = foldl' (collectDepsDecl payload Nothing) HM.empty decls
      variables = map (updateVariableDeps depsMap modName) $ moduleT ^. #variables
      instances = map (updateInstanceDeps depsMap modName) $ moduleT ^. #instances
  in moduleT & #variables .~ variables & #instances .~ instances
collectVarModule _ _other = error $ "Unknown module type " <> show _other

updateVariableDeps :: Map DepsMapKey [Entity] -> String -> VarDesc -> VarDesc
updateVariableDeps depsMap modName var' =
  let key = EntityD $ EntityDef modName (var' ^. #name)
      mDeps = depsMap !? key
  in var' & #dependencies .~ fromMaybe (var' ^. #dependencies) mDeps

updateInstanceDeps :: Map DepsMapKey [Entity] -> String -> InstanceDesc -> InstanceDesc
updateInstanceDeps depsMap modName instance' = instance' & #methods .~ map (updateInstanceMethod depsMap modName (instance' ^. #head)) (instance' ^. #methods)

updateInstanceMethod :: Map DepsMapKey [Entity] -> String -> String -> VarDesc -> VarDesc
updateInstanceMethod depsMap modName instanceHead var' =
  let key = InstanceD $ InstanceDef modName instanceHead
      mDeps = depsMap !? key
  in var' & #dependencies .~ fromMaybe (var' ^. #dependencies) mDeps

collectDepsDecl :: Payload -> Maybe String -> Map DepsMapKey [Entity] -> Decl Src -> Map DepsMapKey [Entity]
collectDepsDecl payload mInstance depsMap (FunBind _ matches) =
  foldr (flip $ collectDepsMatch payload mInstance) depsMap matches
collectDepsDecl payload@Payload{..} mInstance depsMap decl@(PatBind _ (PVar _ name') _ mBinds) =
  let varN = intercalate "." $ prefix <# getName name'
      key = maybe (EntityD $ EntityDef modName varN) (InstanceD . InstanceDef modName) mInstance
      deps = collectVarDecl payload decl $ fromMaybe [] (depsMap !? key)
      depsMap' = HM.insert key deps depsMap
  in maybe depsMap' (collectDepsBinds (payload & #prefix .~ (prefix <# varN)) depsMap') mBinds
collectDepsDecl payload _ depsMap (InstDecl _ _ instRule (Just instanceDecls)) =
  foldr (flip $ collectDepsInstDecl payload instRule) depsMap instanceDecls
collectDepsDecl _ _ depsMap _ = depsMap

collectDepsInstDecl :: Payload -> InstRule Src -> Map DepsMapKey [Entity] -> InstDecl Src -> Map DepsMapKey [Entity]
collectDepsInstDecl payload instRule depsMap (InsDecl _ decl) = collectDepsDecl payload (Just $ prettyPrint instRule) depsMap decl
collectDepsInstDecl _ _ depsMap _ = depsMap

collectDepsMatch :: Payload -> Maybe String -> Map DepsMapKey [Entity] -> Match Src -> Map DepsMapKey [Entity]
collectDepsMatch payload@Payload {..} mInstance depsMap match@(Match _ name' pats _ mBinds) =
  let varN = intercalate "." $ prefix <# getName name'
      key = maybe (EntityD $ EntityDef modName varN) (InstanceD . InstanceDef modName) mInstance
      deps = flip (foldr' $ collectVarPat payload) pats $ collectVarMatch payload match $ fromMaybe [] (depsMap !? key)
      depsMap' = HM.insert key deps depsMap
  in maybe depsMap' (collectDepsBinds (payload & #prefix .~ (prefix <# varN)) depsMap') mBinds
collectDepsMatch payload@Payload {..} mInstance depsMap match@(InfixMatch _ _ name' pats _ mBinds) =
  let varN = intercalate "." $ prefix <# getName name'
      key = maybe (EntityD $ EntityDef modName varN) (InstanceD . InstanceDef modName) mInstance
      deps = flip (foldr' $ collectVarPat payload) pats $ collectVarMatch payload match $ fromMaybe [] (depsMap !? key)
      depsMap' = HM.insert key deps depsMap
  in maybe depsMap' (collectDepsBinds (payload & #prefix .~ (prefix <# varN)) depsMap') mBinds

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
        { name = intercalate "." $ prefix <# getNameFromPat firstPat
        , location = mkRange srcInfo
        , code = prettyPrint $ rmBinds decl
        , dependencies = []
        }
  in Just desc
mkVar prefix decl@(TypeSig srcInfo (name':_) _) =
  let desc = VarDesc
        { name = intercalate "." $ prefix <# getName name'
        , location = mkRange srcInfo
        , code = prettyPrint decl
        , dependencies = []
        }
  in Just desc
mkVar prefix decl@(PatBind srcInfo (PVar _ name') _ _) =
  let desc = VarDesc
        { name = intercalate "." $ prefix <# getName name'
        , location = mkRange srcInfo
        , code = prettyPrint $ rmBinds decl
        , dependencies = []
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
  let prefix = payload ^. #prefix <# getName name'
      payload' = payload & #prefix .~ prefix
  in collectVarRhs payload' rhs res
collectVarDecl _ _ res = res

collectVarMatch :: Payload -> Match Src -> [Entity] -> [Entity]
collectVarMatch payload (Match _ name' pats rhs _) res =
  let prefix = payload ^. #prefix <# getName name'
      localBinds = foldl' (flip $ collectLocalBindsPat payload) (payload ^. #localBindings) pats
      payload' = payload & #prefix .~ prefix & #localBindings .~ localBinds
  in collectVarRhs payload' rhs res
collectVarMatch payload (InfixMatch _ pat name' pats rhs _) res =
  let prefix = payload ^. #prefix <# getName name'
      localBinds = foldl' (flip $ collectLocalBindsPat payload) (payload ^. #localBindings) (pat : pats)
      payload' = payload & #prefix .~ prefix & #localBindings .~ localBinds
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
  let updatedLocalBinds = foldl' (flip $ collectLocalBindsPat payload) (payload ^. #localBindings) pats
      payload' = payload & #localBindings .~ updatedLocalBinds
  in flip (foldr' $ collectVarPat payload) pats $ collectVarExp payload' exp' res
collectVarExp payload (Let _ binds' exp') res =
  let updatedLocalBinds = collectLocalBinds payload binds' (payload ^. #localBindings)
      payload' = payload & #localBindings .~ updatedLocalBinds
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
  let updatedLocalBinds = foldl' (flip $ collectLocalBindsQualStmt payload) (payload ^. #localBindings) qualStmts
      payload' = payload & #localBindings .~ updatedLocalBinds
  in collectVarExp payload' exp' $ foldr (collectVarQualStmt payload') res qualStmts
collectVarExp payload (ParComp _ exp' qualStmts) res =
  let updatedLocalBinds = foldl' (flip $ collectLocalBindsQualStmt payload) (payload ^. #localBindings) (concat qualStmts)
      payload' = payload & #localBindings .~ updatedLocalBinds
  in collectVarExp payload' exp' $ foldr (collectVarQualStmt payload') res (concat qualStmts)
collectVarExp payload (ParArrayComp _ exp' qualStmts) res =
  let updatedLocalBinds = foldl' (flip $ collectLocalBindsQualStmt payload) (payload ^. #localBindings) (concat qualStmts)
      payload' = payload & #localBindings .~ updatedLocalBinds
  in collectVarExp payload' exp' $ foldr (collectVarQualStmt payload') res (concat qualStmts)
collectVarExp payload (ExpTypeSig _ exp' _) res = collectVarExp payload exp' res
collectVarExp payload (Proc _ pat exp') res =
  let updatedLocalBinds = collectLocalBindsPat payload pat (payload ^. #localBindings)
      payload' = payload & #localBindings .~ updatedLocalBinds
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
  let localBinds' = collectLocalBindsPat payload pat (payload ^. #localBindings)
      payload' = payload & #localBindings .~ localBinds'
  in collectVarExp payload exp' $ collectVarStmts payload' rem' res
collectVarStmts payload (Qualifier _ exp' : rem') res = collectVarExp payload exp' $ collectVarStmts payload rem' res
collectVarStmts payload (LetStmt _ binds' : rem') res =
  let updatedLocalBinds = collectLocalBinds payload binds' (payload ^. #localBindings)
      payload' = payload & #localBindings .~ updatedLocalBinds
  in collectVarBinds payload' binds' $ collectVarStmts payload' rem' res
collectVarStmts payload (RecStmt _ stmts : rem') res = collectVarStmts payload stmts $ collectVarStmts payload rem' res

collectVarAlt :: Payload -> Alt Src -> [Entity] -> [Entity]
collectVarAlt payload (Alt _ pat rhs _) res =
  let updatedLocalBinds = collectLocalBindsPat payload pat (payload ^. #localBindings)
      payload' = payload & #localBindings .~ updatedLocalBinds
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
        \ def -> find ((def ^. #name ==) . (^. name_)) ((modulesMap ! (def ^. #_module)) ^. #types) >>=
        getFieldsFromType (getNameQ consQ)
  in maybe localBinds (foldl' (flip HS.insert) localBinds) mFields

getFieldsFromType :: String -> TypeDesc -> Maybe [String]
getFieldsFromType conN (DataT dataT) = find ((conN ==) . (^. name_)) (dataT ^. #constructors) >>= getFieldsFromCons
getFieldsFromType _ (TypeSynT _) = Nothing
getFieldsFromType conN (GadtT gadtT) = find ((conN ==) . (^. name_)) (gadtT ^. #constructors) >>= getFieldsFromCons

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
      moduleM = headMaybe . mapMaybe (lookForVar payload vName . (modulesMap !) . (^. #_module)) . filter (checkImportForVar (Just alias') vName) $ imports
  in mkEntityDef vName <$> moduleM
findEntityDefForVar payload@Payload {..} (UnQual _ vNameT) =
  let vName = getName vNameT
      moduleM = headMaybe . mapMaybe (lookForVar payload vName . (modulesMap !) . (^. #_module)) . filter (checkImportForVar Nothing vName) $ imports
  in if not (HS.member vName localBindings)
    then checkInSelfMod payload vName <|> mkEntityDef vName <$> moduleM
    else Nothing
findEntityDefForVar _ (Special _ _) = Nothing

checkInSelfMod :: Payload -> String -> Maybe Entity
checkInSelfMod payload@Payload {..} varN = checkInSelfModAux prefix []
  <|> (if null prefix then mkEntityDef varN <$> lookForVar payload varN moduleT else Nothing)
  where
  checkInSelfModAux :: [String] -> [String] -> Maybe Entity
  checkInSelfModAux [] curPrefix =
    let varN' = intercalate "." $ curPrefix <# varN
    in if checkForVar True varN' moduleT then Just (mkEntityDef varN' moduleT) else Nothing
  checkInSelfModAux (r:remPrefix) curPrefix =
    let varN' = intercalate "." $ curPrefix <# varN
    in checkInSelfModAux remPrefix (curPrefix <# r) <|> if checkForVar True varN' moduleT then Just (mkEntityDef varN' moduleT) else Nothing

  moduleT :: ModuleT
  moduleT = modulesMap ! modName

mkEntityDef :: String -> ModuleT -> Entity
mkEntityDef vName moduleT = Variable $ EntityDef (moduleT ^. #name) vName

checkImportForVar :: Maybe String -> String -> Import -> Bool
checkImportForVar mAlias varN Import {..} = maybe (not qualified) ((alias ==) . Just) mAlias && matchSpecsForVar specsList varN

matchSpecsForVar :: SpecsList -> String -> Bool
matchSpecsForVar (Include list) varN = any (matchImportItemVar varN) list
matchSpecsForVar (Hide list) varN = not $ any (matchImportItemVar varN) list

matchImportItemVar :: String -> ImportItem -> Bool
matchImportItemVar varN (VarImportItem name') = name' == varN
matchImportItemVar _ _ = False

lookForVar :: Payload -> String -> ModuleT -> Maybe ModuleT
lookForVar payload@Payload {..} varN moduleT =
  let modName' = if checkForVar False varN moduleT
        then Just $ moduleT ^. #name
        else lookForVarInExport payload moduleT varN (moduleT ^. #exports)
  in modName' >>= (modulesMap !?)

lookForVarInExport :: Payload -> ModuleT -> String -> ExportList -> Maybe String
lookForVarInExport _ _ _ AllE = Nothing
lookForVarInExport payload moduleT typeN (SomeE expList) = headMaybe . mapMaybe (lookForVarInExport' payload moduleT typeN) $ expList

lookForVarInExport' :: Payload -> ModuleT -> String -> ExportItem -> Maybe String
lookForVarInExport' payload@Payload {..} moduleT varN ExportVar {name = name', ..}
  | name' == varN =
    let imports' = filter (checkImportForVar qualifier varN) (moduleT ^. #imports)
    in fmap (^. #name) $ headMaybe $ mapMaybe (lookForVar payload varN . (modulesMap !) . (^. #_module)) imports'
  | otherwise = Nothing
lookForVarInExport' payload@Payload {..} moduleT varN ExportModule {name = name'}
  | name' /= moduleT ^. #name =
    case modulesMap !? name' of
      Just moduleT' -> (^. #name) <$> lookForVar payload varN moduleT'
      Nothing ->
        let imports' = filter (checkImportForVar (Just name') varN) (moduleT ^. #imports)
        in fmap (^. #name) $ headMaybe $ mapMaybe (lookForVar payload varN . (modulesMap !) . (^. #_module)) imports'
  | otherwise = Nothing
lookForVarInExport' _ _ _ ExportType {} = Nothing

checkForVar :: Bool -> String -> ModuleT -> Bool
checkForVar selfMod varN moduleT = (selfMod || checkExportListForVar (moduleT ^. #exports) varN) && any ((varN ==) . (^. #name)) (moduleT ^. #variables)

checkExportListForVar :: ExportList -> String -> Bool
checkExportListForVar AllE _ = True
checkExportListForVar (SomeE items) typeN = any (matchExportItemForVar typeN) items

matchExportItemForVar :: String -> ExportItem -> Bool
matchExportItemForVar varN (ExportVar {name = name', ..}) = isNothing qualifier && name' == varN
matchExportItemForVar _ _ = False
