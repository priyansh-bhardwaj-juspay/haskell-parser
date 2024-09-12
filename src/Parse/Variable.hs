module Parse.Variable
  ( collectVarModule
  , mkVarR
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

collectVarModule :: Map String ModuleT -> Module Src -> ModuleT
collectVarModule modulesMap (Module _ (Just (ModuleHead _ (ModuleName _ name') _ _)) _ _ decls) =
  let moduleT = modulesMap ! name'
      payload = Payload
        { modulesMap = modulesMap
        , imports = moduleT ^. #imports
        , modName = name'
        , prefix = []
        , localBindings = HS.empty
        }
      depsMap = foldl' (collectDepsDecl payload) HM.empty decls
      variables = map (\ var' -> var' & #dependencies .~ fromMaybe (var' ^. #dependencies) (depsMap !? (var' ^. #name))) $ moduleT ^. #variables
  in moduleT & #variables .~ variables
collectVarModule _ _other = error $ "Unknown module type " <> show _other

collectDepsDecl :: Payload -> Map String [Entity] -> Decl Src -> Map String [Entity]
collectDepsDecl payload depsMap (FunBind _ matches) =
  foldr (flip $ collectDepsMatch payload) depsMap matches
collectDepsDecl payload@Payload{..} depsMap decl@(PatBind _ (PVar _ name') _ mBinds) =
  let varN = intercalate "|" $ prefix <# getName name'
      deps = collectVarDecl payload decl $ fromMaybe [] (depsMap !? varN)
      depsMap' = HM.insert varN deps depsMap
  in maybe depsMap' (collectDepsBinds (payload & #prefix .~ (prefix <# varN)) depsMap') mBinds
collectDepsDecl _ depsMap _ = depsMap

collectDepsMatch :: Payload -> Map String [Entity] -> Match Src -> Map String [Entity]
collectDepsMatch payload@Payload {..} depsMap match@(Match _ name' _ _ mBinds) =
  let varN = intercalate "|" $ prefix <# getName name'
      deps = collectVarMatch payload match $ fromMaybe [] (depsMap !? varN)
      depsMap' = HM.insert varN deps depsMap
  in maybe depsMap' (collectDepsBinds (payload & #prefix .~ (prefix <# varN)) depsMap') mBinds
collectDepsMatch payload@Payload {..} depsMap match@(InfixMatch _ _ name' _ _ mBinds) =
  let varN = intercalate "|" $ prefix <# getName name'
      deps = collectVarMatch payload match $ fromMaybe [] (depsMap !? varN)
      depsMap' = HM.insert varN deps depsMap
  in maybe depsMap' (collectDepsBinds (payload & #prefix .~ (prefix <# varN)) depsMap') mBinds

collectDepsBinds :: Payload -> Map String [Entity] -> Binds Src -> Map String [Entity]
collectDepsBinds payload depsMap (BDecls _ decls) = foldr (flip $ collectDepsDecl payload) depsMap decls
collectDepsBinds _ depsMap (IPBinds _ _) = depsMap

mkVarR :: [String] -> Decl Src -> [VarDesc] -> [VarDesc]
mkVarR prefix decl res =
  let res' = scanForBinds prefix decl res
  in maybe res' (<:> res') $ mkVar prefix decl

mkVar :: [String] -> Decl Src -> Maybe VarDesc
mkVar prefix decl@(FunBind srcInfo (firstPat:_)) =
  let desc = VarDesc
        { name = intercalate "|" $ prefix <# getNameFromPat firstPat
        , location = mkRange srcInfo
        , code = prettyPrint $ rmBinds decl
        , dependencies = []
        }
  in Just desc
mkVar prefix decl@(TypeSig srcInfo (name':_) _) =
  let desc = VarDesc
        { name = intercalate "|" $ prefix <# getName name'
        , location = mkRange srcInfo
        , code = prettyPrint $ rmBinds decl
        , dependencies = []
        }
  in Just desc
mkVar prefix decl@(PatBind srcInfo (PVar _ name') _ _) =
  let desc = VarDesc
        { name = intercalate "|" $ prefix <# getName name'
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
      localBinds = foldl' (flip collectLocalBindsPat) (payload ^. #localBindings) pats
      payload' = payload & #prefix .~ prefix & #localBindings .~ localBinds
  in collectVarRhs payload' rhs res
collectVarMatch payload (InfixMatch _ pat name' pats rhs _) res =
  let prefix = payload ^. #prefix <# getName name'
      localBinds = foldl' (flip collectLocalBindsPat) (payload ^. #localBindings) (pat : pats)
      payload' = payload & #prefix .~ prefix & #localBindings .~ localBinds
  in collectVarRhs payload' rhs res

collectVarBinds :: Payload -> Binds Src -> [Entity] -> [Entity]
collectVarBinds payload (BDecls _ decls) res = foldr (collectVarDecl payload) res decls
collectVarBinds _ (IPBinds _ _) res = res

collectVarQOp :: Payload -> QOp Src -> [Entity] -> [Entity]
collectVarQOp payload (QVarOp _ qName) res = maybe res (<:res) $ findEntityDefForVar payload qName
collectVarQOp _ (QConOp _ _) res = res

collectVarExp :: Payload -> Exp Src -> [Entity] -> [Entity]
collectVarExp payload (Var _ qName) res = maybe res (<:res) $ findEntityDefForVar payload qName
collectVarExp payload (InfixApp _ exp1 opr exp2) res = collectVarExp payload exp1 $ collectVarQOp payload opr $ collectVarExp payload exp2 res
collectVarExp payload (App _ exp1 exp2) res = collectVarExp payload exp1 $ collectVarExp payload exp2 res
collectVarExp payload (NegApp _ exp') res = collectVarExp payload exp' res
collectVarExp payload (Lambda _ pats exp') res =
  let updatedLocalBinds = foldl' (flip collectLocalBindsPat) (payload ^. #localBindings) pats
      payload' = payload & #localBindings .~ updatedLocalBinds
  in collectVarExp payload' exp' res
collectVarExp payload (Let _ binds' exp') res =
  let updatedLocalBinds = collectLocalBinds binds' (payload ^. #localBindings)
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
  let updatedLocalBinds = foldl' (flip collectLocalBindsQualStmt) (payload ^. #localBindings) qualStmts
      payload' = payload & #localBindings .~ updatedLocalBinds
  in collectVarExp payload' exp' $ foldr (collectVarQualStmt payload') res qualStmts
collectVarExp payload (ParComp _ exp' qualStmts) res =
  let updatedLocalBinds = foldl' (flip collectLocalBindsQualStmt) (payload ^. #localBindings) (concat qualStmts)
      payload' = payload & #localBindings .~ updatedLocalBinds
  in collectVarExp payload' exp' $ foldr (collectVarQualStmt payload') res (concat qualStmts)
collectVarExp payload (ParArrayComp _ exp' qualStmts) res =
  let updatedLocalBinds = foldl' (flip collectLocalBindsQualStmt) (payload ^. #localBindings) (concat qualStmts)
      payload' = payload & #localBindings .~ updatedLocalBinds
  in collectVarExp payload' exp' $ foldr (collectVarQualStmt payload') res (concat qualStmts)
collectVarExp payload (ExpTypeSig _ exp' _) res = collectVarExp payload exp' res
collectVarExp payload (Proc _ pat exp') res =
  let updatedLocalBinds = collectLocalBindsPat pat (payload ^. #localBindings)
      payload' = payload & #localBindings .~ updatedLocalBinds
  in collectVarExp payload' exp' res
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

collectLocalBindsQualStmt :: QualStmt Src -> HS.HashSet String -> HS.HashSet String
collectLocalBindsQualStmt (QualStmt _ (Generator _ pat _)) localBinds = collectLocalBindsPat pat localBinds
collectLocalBindsQualStmt _ localBinds = localBinds

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
  let localBinds' = collectLocalBindsPat pat (payload ^. #localBindings)
      payload' = payload & #localBindings .~ localBinds'
  in collectVarExp payload exp' $ collectVarStmts payload' rem' res
collectVarStmts payload (Qualifier _ exp' : rem') res = collectVarExp payload exp' $ collectVarStmts payload rem' res
collectVarStmts payload (LetStmt _ binds' : rem') res =
  let updatedLocalBinds = collectLocalBinds binds' (payload ^. #localBindings)
      payload' = payload & #localBindings .~ updatedLocalBinds
  in collectVarBinds payload' binds' $ collectVarStmts payload' rem' res
collectVarStmts payload (RecStmt _ stmts : rem') res = collectVarStmts payload stmts $ collectVarStmts payload rem' res

collectVarAlt :: Payload -> Alt Src -> [Entity] -> [Entity]
collectVarAlt payload (Alt _ pat rhs _) res =
  let updatedLocalBinds = collectLocalBindsPat pat (payload ^. #localBindings)
      payload' = payload & #localBindings .~ updatedLocalBinds
  in collectVarRhs payload' rhs res

collectVarRhs :: Payload -> Rhs Src -> [Entity] -> [Entity]
collectVarRhs payload (UnGuardedRhs _ exp') res = collectVarExp payload exp' res
collectVarRhs payload (GuardedRhss _ guardedRhss) res = foldr (collectVarGuardedRhs payload) res guardedRhss

collectVarGuardedRhs :: Payload -> GuardedRhs Src -> [Entity] -> [Entity]
collectVarGuardedRhs payload (GuardedRhs _ stmts exp') res = collectVarExp payload exp' $ foldr (collectVarGuardedRhsStmt payload) res stmts

collectVarGuardedRhsStmt :: Payload -> Stmt Src -> [Entity] -> [Entity]
collectVarGuardedRhsStmt payload (Qualifier _ exp') res = collectVarExp payload exp' res
collectVarGuardedRhsStmt _ _ res = res

collectLocalBindsPat :: Pat Src -> HS.HashSet String -> HS.HashSet String
collectLocalBindsPat (PVar _ name') localBinds = HS.insert (getName name') localBinds
collectLocalBindsPat (PInfixApp _ pat1 _ pat2) localBinds = collectLocalBindsPat pat1 $ collectLocalBindsPat pat2 localBinds
collectLocalBindsPat (PApp _ _ pats) localBinds = foldl' (flip collectLocalBindsPat) localBinds pats
collectLocalBindsPat (PTuple _ _ pats) localBinds = foldl' (flip collectLocalBindsPat) localBinds pats
collectLocalBindsPat (PUnboxedSum _ _ _ pat) localBinds = collectLocalBindsPat pat localBinds
collectLocalBindsPat (PList _ pats) localBinds = foldl' (flip collectLocalBindsPat) localBinds pats
collectLocalBindsPat (PParen _ pat) localBinds = collectLocalBindsPat pat localBinds
collectLocalBindsPat (PRec _ _ patFields) localBinds = foldl' (flip collectLocalBindsPatField) localBinds patFields
collectLocalBindsPat (PAsPat _ name' pat) localBinds = collectLocalBindsPat pat $ HS.insert (getName name') localBinds
collectLocalBindsPat (PatTypeSig _ pat _) localBinds = collectLocalBindsPat pat localBinds
collectLocalBindsPat (PViewPat _ _ pat) localBinds = collectLocalBindsPat pat localBinds
collectLocalBindsPat (PBangPat _ pat) localBinds = collectLocalBindsPat pat localBinds
collectLocalBindsPat _ localBinds = localBinds

collectLocalBindsPatField :: PatField Src -> HS.HashSet String -> HS.HashSet String
collectLocalBindsPatField (PFieldPat _ _ pat) localBinds = collectLocalBindsPat pat localBinds
collectLocalBindsPatField (PFieldPun _ qName) localBinds = collectLocalBindsQName qName localBinds
collectLocalBindsPatField (PFieldWildcard _) localBinds = localBinds -- TODO: Use Types data to bind all fields

collectLocalBindsQName ::QName Src -> HS.HashSet String -> HS.HashSet String
collectLocalBindsQName (Qual _ _ name') localBinds = HS.insert (getName name') localBinds
collectLocalBindsQName (UnQual _ name') localBinds = HS.insert (getName name') localBinds
collectLocalBindsQName _ localBinds = localBinds

collectLocalBinds :: Binds Src -> HS.HashSet String -> HS.HashSet String
collectLocalBinds (BDecls _ decls) localBinds = foldl' (flip collectLocalBindsDecl) localBinds decls
collectLocalBinds _ localBinds = localBinds

collectLocalBindsDecl :: Decl Src -> HS.HashSet String -> HS.HashSet String
collectLocalBindsDecl (PatBind _ pat _ _) localBinds = collectLocalBindsPat pat localBinds
collectLocalBindsDecl (FunBind _ matches) localBinds = foldl' (flip collectLocalBindsMatch) localBinds matches
collectLocalBindsDecl _ localBinds = localBinds

collectLocalBindsMatch :: Match Src -> HS.HashSet String -> HS.HashSet String
collectLocalBindsMatch (Match _ name' _ _ _) localBinds = HS.insert (getName name') localBinds
collectLocalBindsMatch (InfixMatch _ _ name' _ _ _) localBinds = HS.insert (getName name') localBinds

findEntityDefForVar :: Payload -> QName SrcSpanInfo -> Maybe Entity
findEntityDefForVar payload qName = Variable <$> findEntityDefForVar' payload qName

findEntityDefForVar' :: Payload -> QName SrcSpanInfo -> Maybe EntityDef
findEntityDefForVar' payload@Payload {..} (Qual _ (ModuleName _ alias') vNameT) =
  let vName = getName vNameT
      moduleM = headMaybe . mapMaybe (lookForVar payload vName . (modulesMap !) . (^. #_module)) . filter (checkImportForVar (Just alias') vName) $ imports
  in mkEntitiDef vName <$> moduleM
findEntityDefForVar' payload@Payload {..} (UnQual _ vNameT) =
  let vName = getName vNameT
      moduleM = headMaybe . mapMaybe (lookForVar payload vName . (modulesMap !) . (^. #_module)) . filter (checkImportForVar Nothing vName) $ imports
  in if not (HS.member vName localBindings)
    then checkInSelfMod payload vName <|> mkEntitiDef vName <$> moduleM
    else Nothing
findEntityDefForVar' _ (Special _ _) = Nothing

checkInSelfMod :: Payload -> String -> Maybe EntityDef
checkInSelfMod payload@Payload {..} varN = checkInSelfModAux prefix []
  <|> (if null prefix then mkEntitiDef varN <$> lookForVar payload varN moduleT else Nothing)
  where
  checkInSelfModAux :: [String] -> [String] -> Maybe EntityDef
  checkInSelfModAux [] curPrefix =
    let varN' = intercalate "|" $ curPrefix <# varN
    in if checkForVar True varN' moduleT then Just (mkEntityDef varN') else Nothing
  checkInSelfModAux (r:remPrefix) curPrefix =
    let varN' = intercalate "|" $ curPrefix <# varN
    in checkInSelfModAux remPrefix (curPrefix <# r) <|> if checkForVar True varN' moduleT then Just (mkEntityDef varN') else Nothing

  mkEntityDef :: String -> EntityDef
  mkEntityDef = EntityDef modName

  moduleT :: ModuleT
  moduleT = modulesMap ! modName

mkEntitiDef :: String -> ModuleT -> EntityDef
mkEntitiDef vName moduleT = EntityDef (moduleT ^. #name) vName

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
