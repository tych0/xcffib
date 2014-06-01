{-# LANGUAGE ViewPatterns #-}
module Data.XCB.Python.Parse (
  parse,
  xform,
  renderPy
  ) where

import Control.Monad.State.Strict

import Data.Either
import Data.List
import qualified Data.Map as M
import Data.Tree
import Data.Maybe
import Data.XCB.FromXML
import Data.XCB.Types as X
import Data.XCB.Python.PyHelpers

import Language.Python.Common as P

import System.FilePath.Glob

import Debug.Trace

data TypeInfo =
  -- | A "base" X type, i.e. one described in baseTypeInfo; first arg is the
  -- struct.unpack string, second is the size.
  BaseType String Int |
  -- | A composite type, i.e. a Struct or Union created by XCB. First arg is
  -- the extension that defined it, second is the name of the type, third arg
  -- is the size if it is known.
  CompositeType String String (Maybe Int)
  deriving (Eq, Ord, Show)

type TypeInfoMap = M.Map X.Type TypeInfo

data BindingPart =
  Request (Statement ()) (Suite ()) |
  Declaration (Suite ()) |
  Noop
  deriving (Show)

collectBindings :: [BindingPart] -> (Suite (), Suite ())
collectBindings = foldr collectR ([], [])
  where
    collectR :: BindingPart -> (Suite (), Suite ()) -> (Suite (), Suite ())
    collectR (Request def decl) (defs, decls) = (def : defs, decl ++ decls)
    collectR (Declaration decl) (defs, decls) = (defs, decl ++ decls)
    collectR Noop x = x

parse :: FilePath -> IO [XHeader]
parse fp = do
  files <- globDir1 (compile "*") fp
  fromFiles files

renderPy :: Suite () -> String
renderPy s = ((intercalate "\n") $ map prettyText s) ++ "\n"

-- | Generate the code for a set of X headers. Note that the code is generated
-- in dependency order, NOT in the order you pass them in. Thus, you get a
-- string (a suggested filename) along with the python code for that XHeader
-- back.
xform :: [XHeader] -> [(String, Suite ())]
xform = map buildPython . dependencyOrder
  where
    buildPython :: Tree XHeader -> (String, Suite ())
    buildPython forest =
      let forest' = (mapM processXHeader $ postOrder forest)
          results = evalState forest' baseTypeInfo
      in last results
    processXHeader :: XHeader
                   -> State TypeInfoMap (String, Suite ())
    processXHeader header = do
      let imports = [mkImport "xcffib", mkImport "struct", mkImport "cStringIO"]
          version = mkVersion header
          key = maybeToList $ mkKey header
          globals = [mkDict "_events", mkDict "_errors"]
          name = xheader_header header
          add = [mkAddExt header]
      parts <- mapM (processXDecl name) $ xheader_decls header
      let (requests, decls) = collectBindings parts
          ext = if length requests > 0
                then [mkClass (name ++ "Extension") "xcffib.Extension" requests]
                else []
      return $ (name, concat [imports, version, key, globals, decls, ext, add])
    -- Rearrange the headers in dependency order for processing (i.e. put
    -- modules which import others after the modules they import, so typedefs
    -- are propogated appropriately).
    dependencyOrder :: [XHeader] -> Forest XHeader
    dependencyOrder headers = unfoldForest unfold $ map xheader_header headers
      where
        headerM = M.fromList $ map (\h -> (xheader_header h, h)) headers
        unfold s = let h = headerM M.! s in (h, deps h)
        deps :: XHeader -> [String]
        deps = catMaybes . map matchImport . xheader_decls
        matchImport :: XDecl -> Maybe String
        matchImport (XImport n) = Just n
        matchImport _ = Nothing
        headerCmp h1 h2 = (xheader_header h1) == (xheader_header h2)
    postOrder :: Tree a -> [a]
    postOrder (Node e cs) = (concat $ map postOrder cs) ++ [e]


mkAddExt :: XHeader -> Statement ()
mkAddExt (xheader_header -> "xproto") =
  flip StmtExpr () $ mkCall "xcffib._add_core" [ mkName "xprotoExtension"
                                               , mkName "Setup"
                                               , mkName "_events"
                                               , mkName "_errors"
                                               ]
mkAddExt header =
  let name = xheader_header header
      ext = mkCall "xcffib.ExtensionKey" [ mkStr name ]
  in flip StmtExpr () $ mkCall "xcffib._add_ext" [ ext
                                                 , mkName (name ++ "Extension")
                                                 , mkName "_events"
                                                 , mkName "_errors"
                                                 ]

-- | Information on basic X types.
baseTypeInfo :: TypeInfoMap
baseTypeInfo = M.fromList $
  [ (UnQualType "CARD8",    BaseType "B" 1)
  , (UnQualType "uint8_t",  BaseType "B" 1)
  , (UnQualType "CARD16",   BaseType "H" 2)
  , (UnQualType "uint16_t", BaseType "H" 2)
  , (UnQualType "CARD32",   BaseType "I" 4)
  , (UnQualType "uint32_t", BaseType "I" 4)
  , (UnQualType "CARD64",   BaseType "Q" 8)
  , (UnQualType "uint64_t", BaseType "Q" 8)
  , (UnQualType "INT8",     BaseType "b" 1)
  , (UnQualType "int8_t",   BaseType "b" 1)
  , (UnQualType "INT16",    BaseType "h" 2)
  , (UnQualType "int16_t",  BaseType "h" 2)
  , (UnQualType "INT32",    BaseType "i" 4)
  , (UnQualType "int32_t",  BaseType "i" 4)
  , (UnQualType "INT64",    BaseType "q" 8)
  , (UnQualType "uint64_t", BaseType "q" 8)
  , (UnQualType "BYTE",     BaseType "B" 1)
  , (UnQualType "BOOL",     BaseType "B" 1)
  , (UnQualType "char",     BaseType "b" 1)
  , (UnQualType "void",     BaseType "B" 1)
  , (UnQualType "float",    BaseType "f" 4)
  , (UnQualType "double",   BaseType "d" 8)
  ]

xBinopToPyOp :: X.Binop -> P.Op ()
xBinopToPyOp X.Add = P.Plus ()
xBinopToPyOp X.Sub = P.Minus ()
xBinopToPyOp X.Mult = P.Multiply ()
xBinopToPyOp X.Div = P.Divide ()
xBinopToPyOp X.And = P.And ()
xBinopToPyOp X.RShift = P.ShiftRight ()

xUnopToPyOp :: X.Unop -> P.Op ()
xUnopToPyOp X.Complement = P.Invert ()

xExpressionToPyExpr :: XExpression -> Expr ()
xExpressionToPyExpr (Value i) = mkInt i
xExpressionToPyExpr (Bit i) = BinaryOp (ShiftLeft ()) (mkInt 1) (mkInt i) ()
xExpressionToPyExpr (FieldRef n) = mkAttr n
xExpressionToPyExpr (EnumRef _ n) = mkVar n
xExpressionToPyExpr (PopCount e) =
  mkCall "xcffib.popcount" [xExpressionToPyExpr e]
-- http://cgit.freedesktop.org/xcb/proto/tree/doc/xml-xcb.txt#n290
xExpressionToPyExpr (SumOf n) = mkCall "sum" [mkAttr n]
xExpressionToPyExpr (Op o e1 e2) =
  let o' = xBinopToPyOp o
      e1' = xExpressionToPyExpr e1
      e2' = xExpressionToPyExpr e2
  in BinaryOp o' e1' e2' ()
xExpressionToPyExpr (Unop o e) =
  let o' = xUnopToPyOp o
      e' = xExpressionToPyExpr e
  in UnaryOp o' e' ()

xEnumElemsToPyEnum :: [XEnumElem] -> [(String, Expr ())]
xEnumElemsToPyEnum membs = reverse $ conv membs [] [1..]
  where
    conv :: [XEnumElem] -> [(String, Expr ())] -> [Int] -> [(String, Expr ())]
    conv ((EnumElem name expr) : els) acc is =
      let expr' = fromMaybe (mkInt (head is)) $ fmap xExpressionToPyExpr expr
          is' = tail is
          acc' = (name, expr') : acc
      in conv els acc' is'
    conv [] acc _ = acc

structElemToPyUnpack :: String
                     -> TypeInfoMap
                     -> GenStructElem Type
                     -> Either (Maybe String, String, Maybe Int)
                               (Statement (), Expr ())
structElemToPyUnpack _ _ (Pad i) = Left (Nothing, (show i) ++ "x", Just i)

-- XXX: This is a cheap hack for noop, we should really do better.
structElemToPyUnpack _ _ (Doc _ _ _) = Left (Nothing, "", Nothing)
-- XXX: What does fd/switch mean? we should implement it correctly
structElemToPyUnpack _ _ (Fd _) = Left (Nothing, "", Nothing)
structElemToPyUnpack _ _ (Switch _ _ _) = Left (Nothing, "", Nothing)

-- The enum field is mostly for user information, so we ignore it.
structElemToPyUnpack ext m (X.List n typ len _) =
  -- -1 is the "I don't know" List sentinel, by XCB convention. We should
  -- probably switch this to None.
  let len' = fromMaybe (mkInt (-1)) $ fmap xExpressionToPyExpr len
      (cons, sz) = case m M.! typ of
                     BaseType c i -> (mkStr c, Just i)
                     CompositeType tExt c i | ext /= tExt ->
                       (mkName $ tExt ++ "." ++ c, i)
                     CompositeType _ c i -> (mkName c, i)
      size = map mkInt $ maybeToList sz
      list = mkCall "xcffib.List" ([ (mkName "parent")
                                   , (mkName "offset")
                                   , len'
                                   , cons
                                   ] ++ size)
      assign = mkAssign (mkAttr n) list
      totalBytes = mkAttr (n ++ ".bufsize")
  in Right (assign, totalBytes)

-- The mask and enum fields are for user information, we can ignore them here.
structElemToPyUnpack ext m (SField n typ _ _) =
  case m M.! typ of
    BaseType c i -> Left (Just n, c, Just i)
    CompositeType tExt c i ->
      let c' = if tExt == ext then c else tExt ++ "." ++ c
          size = map mkInt $ maybeToList i
          assign = mkAssign (mkAttr n) (mkCall c' ([ mkName "parent"
                                                   , mkName "offset"
                                                   ] ++ size))
      in Right (assign, mkAttr (n ++ ".buflen"))
structElemToPyUnpack _ _ (ExprField _ _ _) = error "Only valid for requests"
structElemToPyUnpack _ _ (ValueParam _ _ _ _) = error "Only valid for requests"

structElemToPyPack :: String
                   -> TypeInfoMap
                   -> GenStructElem Type
                   -> Either (Maybe String, String) ([String], Expr ())
structElemToPyPack _ _ (Pad i) = Left (Nothing, (show i) ++ "x")
-- TODO: implement doc, switch, and fd?
structElemToPyPack _ _ (Doc _ _ _) = Left (Nothing, "")
structElemToPyPack _ _ (Switch _ _ _) = Left (Nothing, "")
structElemToPyPack _ _ (Fd _) = Left (Nothing, "")
structElemToPyPack _ m (SField n typ _ _) =
  case m M.! typ of
    BaseType c _ -> Left (Just n, c)
    -- TODO: be a little smarter here; we should really make sure that things
    -- have a .pack(); if users are calling us via the old style api, we need
    -- to support that as well.
    CompositeType _ _ _ -> Right $ ([n], mkCall (n ++ ".pack") [])
-- TODO: assert values are in enum?
structElemToPyPack ext m (X.List n typ len _) =
  case m M.! typ of
    BaseType c i -> Right $ ([n], mkCall "xcffib.pack_list" [ mkName n
                                                            , mkStr c
                                                            , mkInt i
                                                            ])
    CompositeType tExt c i ->
      let c' = if tExt == ext then c else (tExt ++ "." ++ c)
          size = maybeToList $ fmap mkInt i
      in Right $ ([n], mkCall "xcffib.pack_list" ([ mkName n
                                                  , mkName c'
                                                  ] ++ size))
structElemToPyPack _ m (ExprField name typ expr) =
  let e = xExpressionToPyExpr expr
  in case m M.! typ of
       BaseType c _ -> Right $ ([name], mkCall "struct.pack" [ mkStr c
                                                             , e
                                                             ])
       CompositeType _ _ _ -> Right $ ([name],
                                       mkCall (mkDot e (mkName "pack")) [])
structElemToPyPack ext m (ValueParam typ mask padding list) =
  case m M.! typ of
    BaseType c i ->
      let mask' = mkCall "struct.pack" [mkStr c, mkName mask]
          list' = mkCall "xcffib.pack_list" [ mkName list
                                            , mkStr "I"
                                            , mkInt i
                                            ]
          toWrite = BinaryOp (Plus ()) mask' list' ()
      in Right $ ([mask, list], mkCall "buf.write" [toWrite])
    CompositeType _ _ _ -> error (
      "ValueParams other than CARD{16,32} not allowed.")

-- | Make a struct style (i.e. not union style) unpack.
mkStructStyleUnpack :: String
                    -> TypeInfoMap
                    -> [GenStructElem Type]
                    -> (Suite (), Maybe Int)
-- TODO: Generate pack() method.
mkStructStyleUnpack ext m membs =
  let unpackF = structElemToPyUnpack ext m
      (toUnpack, lists) = partitionEithers $ map unpackF membs
      -- XXX: Here we assume that all the lists come after all the unpacked
      -- members. While (I think) this is true today, it may not always be
      -- true and we should probably fix this.
      (names, packs, lengths) = unzip3 toUnpack
      names' = catMaybes names
      assign = mkUnpackFrom names' packs
      unpackLength = sum $ catMaybes lengths
      incr = mkIncr "offset" $ mkInt unpackLength
      baseTUnpack = if length names' > 0 then [assign] else []
      baseTIncr = if unpackLength > 0 then [incr] else []

      lists' = concat $ map (\(l, sz) -> [l, mkIncr "offset" sz]) lists

      statements = baseTUnpack ++ baseTIncr ++ lists'
      structLen = if length lists > 0 then Nothing else Just unpackLength
  in (statements, structLen)

-- | Given a (qualified) type name and a target type, generate a TypeInfoMap
-- updater.
mkModify :: String -> String -> TypeInfo -> TypeInfoMap -> TypeInfoMap
mkModify ext name ti m =
  let m' = M.fromList [ (UnQualType name, ti)
                      , (QualType ext name, ti)
                      ]
  in M.union m m'

processXDecl :: String
             -> XDecl
             -> State TypeInfoMap BindingPart
processXDecl ext (XTypeDef name typ) =
  do modify $ \m -> mkModify ext name (m M.! typ) m
     return Noop
processXDecl ext (XidType name) =
  -- http://www.markwitmer.com/guile-xcb/doc/guile-xcb/XIDs.html
  do modify $ mkModify ext name (BaseType "I" 4)
     return Noop
processXDecl _ (XImport n) =
  return $ Declaration [mkImport n]
processXDecl _ (XEnum name membs) =
  return $ Declaration [mkEnum name $ xEnumElemsToPyEnum membs]
processXDecl ext (XStruct n membs) = do
  m <- get
  let (statements, structLen) = mkStructStyleUnpack ext m membs
  modify $ mkModify ext n (CompositeType ext n structLen)
  return $ Declaration [mkXClass n "xcffib.Struct" statements]
processXDecl ext (XEvent name number membs hasSequence) = do
  -- TODO: if not hasSequence then we increment by 1 byte? see KeymapNotify
  m <- get
  let cname = name ++ "Event"
      (statements, _) = mkStructStyleUnpack ext m membs
      eventsUpd = mkDictUpdate "_events" number cname
  return $ Declaration [mkXClass cname "xcffib.Event" statements, eventsUpd]
processXDecl ext (XError name number membs) = do
  m <- get
  let cname = name ++ "Error"
      (statements, _) = mkStructStyleUnpack ext m membs
      errorsUpd = mkDictUpdate "_events" number cname
  return $ Declaration [mkXClass cname "xcffib.Error" statements, errorsUpd]
processXDecl ext (XRequest name number membs reply) = do
  m <- get
  -- Requests with lists not supported yet
  let packF = structElemToPyPack ext m
      (toPack, stmts) = partitionEithers $ map packF membs
      (listNames, lists) = let (lns, ls) = unzip stmts in (concat lns, ls)
      lists' = map (flip StmtExpr ()) lists
      (args, keys) = unzip toPack
      args' =
        let theArgs = (catMaybes args) ++ listNames
        in case (ext, name) of
             -- XXX: The 1.10 ConfigureWindow definiton has value_mask
             -- explicitly listed in the protocol definition, but everywhere
             -- else it isn't, but everywhere else it isn't; to keep things
             -- uniform, we remove it here.
             ("xproto", "ConfigureWindow") -> nub $ theArgs
             _ -> theArgs
      buf = mkAssign "buf" (mkCall "cStringIO.StringIO" [])
      -- TODO: prefix with =xx2x as in xpyb?
      packStr = mkStr $ intercalate "" keys
      write = mkCall "buf.write" [mkCall "struct.pack"
                                         ([packStr] ++ (map mkName args'))]
      writeStmt = if length args > 0 then [StmtExpr write ()] else []
      ret = mkReturn $ mkCall "self.send_request" [mkInt number, mkName "buf"]
      -- TODO: checked vs. unchecked?
      requestBody = [buf] ++ writeStmt ++ lists' ++ [ret]
      request = mkMethod name ("self" : args') requestBody

      replyDecl = concat $ maybeToList $ do
        reply' <- reply
        let (replyStatements, _) = mkStructStyleUnpack ext m reply'
            replyName = name ++ "Reply"
            theReply = mkXClass replyName "xcffib.Reply" replyStatements
            cookie = mkEmptyClass (name ++ "Cookie") "xcffib.Cookie"
        return [theReply, cookie]
  return $ Request request replyDecl
processXDecl ext (XUnion name membs) = do
  m <- get
  let unpackF = structElemToPyUnpack ext m
      (fields, lists) = partitionEithers $ map unpackF membs
      (toUnpack, sizes) = unzip $ map mkUnionUnpack fields
      (lists', lengths) = unzip lists
      err = error ("bad XCB: union " ++
                   name ++ " has fields of different length")
      lengths' = catMaybes $ nub sizes
      unionLen = if length lists' > 0 then Nothing else listToMaybe lengths'
      decl = [mkXClass name "xcffib.Union" $ (fst $ unzip lists) ++ toUnpack]
  -- There should be at most one size of object in the struct.
  unless ((length $ lengths') <= 1) err
  -- List in list, so we don't know a length here. -1 is the sentinel value
  -- xpyb uses for this.
  modify $ mkModify ext name (CompositeType ext name unionLen)
  return $ Declaration decl
  where
    mkUnionUnpack :: (Maybe String, String, Maybe Int)
                  -> (Statement (), Maybe Int)
    mkUnionUnpack (name, typ, size) =
      (mkUnpackFrom (maybeToList name) [typ], size)

processXDecl ext (XidUnion name _) =
  -- These are always unions of only XIDs.
  do modify $ mkModify ext name (BaseType "I" 4)
     return Noop

mkVersion :: XHeader -> Suite ()
mkVersion header =
  let major = ver "MAJOR_VERSION" (xheader_major_version header)
      minor = ver "MINOR_VERSION" (xheader_minor_version header)
  in major ++ minor
  where
    ver :: String -> Maybe Int -> Suite ()
    ver target i = maybeToList $ fmap (\x -> mkAssign target (mkInt x)) i

mkKey :: XHeader -> Maybe (Statement ())
mkKey header = do
  name <- xheader_xname header
  let call = mkCall "xcffib.ExtensionKey" [mkStr name]
  return $ mkAssign "key" call
