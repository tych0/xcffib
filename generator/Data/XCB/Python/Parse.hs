{-# LANGUAGE ViewPatterns #-}
module Data.XCB.Python.Parse (
  parse,
  xform,
  renderPy
  ) where

import Control.Monad.State.Strict

import Data.Either
import Data.List
import Data.List.Utils
import qualified Data.Map as M
import Data.Tree
import Data.Maybe
import Data.XCB.FromXML
import Data.XCB.Types as X
import Data.XCB.Python.PyHelpers

import Language.Python.Common as P

import System.FilePath.Glob

import Text.Format

data TypeInfo =
  -- | A "base" X type, i.e. one described in baseTypeInfo; first arg is the
  -- struct.unpack string, second is the size.
  BaseType String |
  -- | A composite type, i.e. a Struct or Union created by XCB. First arg is
  -- the extension that defined it, second is the name of the type, third arg
  -- is the size if it is known.
  CompositeType String String
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
  files <- globDir1 (compile "*.xml") fp
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
      let imports = [mkImport "xcffib", mkImport "struct", mkImport "six"]
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
  in flip StmtExpr () $ mkCall "xcffib._add_ext" [ mkName "key"
                                                 , mkName (name ++ "Extension")
                                                 , mkName "_events"
                                                 , mkName "_errors"
                                                 ]

-- | Information on basic X types.
baseTypeInfo :: TypeInfoMap
baseTypeInfo = M.fromList $
  [ (UnQualType "CARD8",    BaseType "B")
  , (UnQualType "uint8_t",  BaseType "B")
  , (UnQualType "CARD16",   BaseType "H")
  , (UnQualType "uint16_t", BaseType "H")
  , (UnQualType "CARD32",   BaseType "I")
  , (UnQualType "uint32_t", BaseType "I")
  , (UnQualType "CARD64",   BaseType "Q")
  , (UnQualType "uint64_t", BaseType "Q")
  , (UnQualType "INT8",     BaseType "b")
  , (UnQualType "int8_t",   BaseType "b")
  , (UnQualType "INT16",    BaseType "h")
  , (UnQualType "int16_t",  BaseType "h")
  , (UnQualType "INT32",    BaseType "i")
  , (UnQualType "int32_t",  BaseType "i")
  , (UnQualType "INT64",    BaseType "q")
  , (UnQualType "uint64_t", BaseType "q")
  , (UnQualType "BYTE",     BaseType "B")
  , (UnQualType "BOOL",     BaseType "B")
  , (UnQualType "char",     BaseType "c")
  , (UnQualType "void",     BaseType "c")
  , (UnQualType "float",    BaseType "f")
  , (UnQualType "double",   BaseType "d")
  ]

xBinopToPyOp :: X.Binop -> P.Op ()
xBinopToPyOp X.Add = P.Plus ()
xBinopToPyOp X.Sub = P.Minus ()
xBinopToPyOp X.Mult = P.Multiply ()
xBinopToPyOp X.Div = P.Divide ()
xBinopToPyOp X.And = P.BinaryAnd ()
xBinopToPyOp X.RShift = P.ShiftRight ()

xUnopToPyOp :: X.Unop -> P.Op ()
xUnopToPyOp X.Complement = P.Invert ()

xExpressionToPyExpr :: (String -> String) -> XExpression -> Expr ()
xExpressionToPyExpr _ (Value i) = mkInt i
xExpressionToPyExpr _ (Bit i) = BinaryOp (ShiftLeft ()) (mkInt 1) (mkInt i) ()
xExpressionToPyExpr acc (FieldRef n) = mkName $ acc n
xExpressionToPyExpr _ (EnumRef _ n) = mkName n
xExpressionToPyExpr acc (PopCount e) =
  mkCall "xcffib.popcount" [xExpressionToPyExpr acc e]
-- http://cgit.freedesktop.org/xcb/proto/tree/doc/xml-xcb.txt#n290
xExpressionToPyExpr acc (SumOf n) = mkCall "sum" [mkName $ acc n]
xExpressionToPyExpr acc (Op o e1 e2) =
  let o' = xBinopToPyOp o
      e1' = xExpressionToPyExpr acc e1
      e2' = xExpressionToPyExpr acc e2
  in BinaryOp o' e1' e2' ()
xExpressionToPyExpr acc (Unop o e) =
  let o' = xUnopToPyOp o
      e' = xExpressionToPyExpr acc e
  in UnaryOp o' e' ()

xEnumElemsToPyEnum :: (String -> String) -> [XEnumElem] -> [(String, Expr ())]
xEnumElemsToPyEnum accessor membs = reverse $ conv membs [] [1..]
  where
    exprConv = xExpressionToPyExpr accessor
    conv :: [XEnumElem] -> [(String, Expr ())] -> [Int] -> [(String, Expr ())]
    conv ((EnumElem name expr) : els) acc is =
      let expr' = fromMaybe (mkInt (head is)) $ fmap exprConv expr
          is' = tail is
          acc' = (name, expr') : acc
      in conv els acc' is'
    conv [] acc _ = acc


-- Add the xcb_generic_{request,reply}_t structure data to the beginning of a
-- pack string. This is a little weird because both structs contain a one byte
-- pad which isn't at the end. If the first element of the request or reply is
-- a byte long, it takes that spot instead, and there is one less offset
addStructData :: String -> String -> String
addStructData prefix (c : cs) | c `elem` "Bbx" =
  let result = (format prefix [[c]])
  in if result == prefix then result ++ (c : cs) else result ++ cs
addStructData prefix s = (format prefix ["x"]) ++ s


-- Don't prefix a single pad byte with a '1'. This is simpler to parse
-- visually, and also simplifies addStructData above.
mkPad :: Int -> String
mkPad 1 = "x"
mkPad i = (show i) ++ "x"

structElemToPyUnpack :: String
                     -> TypeInfoMap
                     -> GenStructElem Type
                     -> Either (Maybe String, String)
                               (Statement ())
structElemToPyUnpack _ _ (Pad i) = Left (Nothing, mkPad i)

-- XXX: This is a cheap hack for noop, we should really do better.
structElemToPyUnpack _ _ (Doc _ _ _) = Left (Nothing, "")
-- XXX: What does fd/switch mean? we should implement it correctly
structElemToPyUnpack _ _ (Fd _) = Left (Nothing, "")
structElemToPyUnpack _ _ (Switch _ _ _) = Left (Nothing, "")

-- The enum field is mostly for user information, so we ignore it.
structElemToPyUnpack ext m (X.List n typ len _) =
  let attr = ((++) "self.")
      len' = fromMaybe pyNone $ fmap (xExpressionToPyExpr attr) len
      cons = case m M.! typ of
               BaseType c -> mkStr c
               CompositeType tExt c | ext /= tExt -> mkName $ tExt ++ "." ++ c
               CompositeType _ c -> mkName c
      list = mkCall "xcffib.List" [ (mkName "unpacker")
                                  , cons
                                  , len'
                                  ]
      assign = mkAssign (mkAttr n) list
  in Right assign

-- The mask and enum fields are for user information, we can ignore them here.
structElemToPyUnpack ext m (SField n typ _ _) =
  case m M.! typ of
    BaseType c -> Left (Just n, c)
    CompositeType tExt c ->
      let c' = if tExt == ext then c else tExt ++ "." ++ c
          assign = mkAssign (mkAttr n) (mkCall c' [mkName "unpacker"])
      in Right assign
structElemToPyUnpack _ _ (ExprField _ _ _) = error "Only valid for requests"
structElemToPyUnpack _ _ (ValueParam _ _ _ _) = error "Only valid for requests"

structElemToPyPack :: String
                   -> TypeInfoMap
                   -> (String -> String)
                   -> GenStructElem Type
                   -> Either (Maybe String, String) ([String], Expr ())
structElemToPyPack _ _ _ (Pad i) = Left (Nothing, mkPad i)
-- TODO: implement doc, switch, and fd?
structElemToPyPack _ _ _ (Doc _ _ _) = Left (Nothing, "")
structElemToPyPack _ _ _ (Switch _ _ _) = Left (Nothing, "")
structElemToPyPack _ _ _ (Fd _) = Left (Nothing, "")
structElemToPyPack _ m accessor (SField n typ _ _) =
  let name = accessor n
  in case m M.! typ of
       BaseType c -> Left (Just name, c)
       -- XXX: be a little smarter here? we should really make sure that things
       -- have a .pack(); if users are calling us via the old style api, we need
       -- to support that as well. This isn't super necessary, though, because
       -- currently (xcb-proto 1.10) there are no direct packs of raw structs, so
       -- this is really only necessary if xpyb gets forward ported in the future if
       -- there are actually calls of this type.
       CompositeType _ _ -> Right $ ([name], mkCall (name ++ ".pack") noArgs)
-- TODO: assert values are in enum?
structElemToPyPack ext m accessor (X.List n typ _ _) =
  let name = accessor n
  in case m M.! typ of
        BaseType c -> Right $ ([name], mkCall "xcffib.pack_list" [ mkName $ name
                                                                 , mkStr c
                                                                 ])
        CompositeType tExt c ->
          let c' = if tExt == ext then c else (tExt ++ "." ++ c)
          in Right $ ([name], mkCall "xcffib.pack_list" ([ mkName $ name
                                                         , mkName c'
                                                         ]))
structElemToPyPack _ m accessor (ExprField name typ expr) =
  let e = (xExpressionToPyExpr accessor) expr
      name' = accessor name
  in case m M.! typ of
       BaseType c -> Right $ ([name'], mkCall "struct.pack" [ mkStr ('=' : c)
                                                            , e
                                                            ])
       CompositeType _ _ -> Right $ ([name'],
                                     mkCall (mkDot e (mkName "pack")) noArgs)

-- As near as I can tell here the padding param is unused.
structElemToPyPack _ m accessor (ValueParam typ mask _ list) =
  case m M.! typ of
    BaseType c ->
      let mask' = mkCall "struct.pack" [mkStr ('=' : c), mkName $ accessor mask]
          list' = mkCall "xcffib.pack_list" [ mkName $ accessor list
                                            , mkStr "I"
                                            ]
          toWrite = BinaryOp (Plus ()) mask' list' ()
      in Right $ ([mask, list], toWrite)
    CompositeType _ _ -> error (
      "ValueParams other than CARD{16,32} not allowed.")

mkPackStmts :: String
            -> String
            -> TypeInfoMap
            -> (String -> String)
            -> String
            -> [GenStructElem Type]
            -> ([String], Suite ())
mkPackStmts ext name m accessor prefix membs =
  let buf = [mkAssign "buf" (mkCall "six.BytesIO" noArgs)]
      packF = structElemToPyPack ext m accessor
      (toPack, stmts) = partitionEithers $ map packF membs
      (listNames, lists) = let (lns, ls) = unzip stmts in (concat lns, ls)
      lists' = map (flip StmtExpr () . mkCall "buf.write" . (: [])) lists
      (args, keys) = unzip toPack
      args' = catMaybes args
      methodArgs =
        let theArgs = args' ++ listNames
        in case (ext, name) of
             -- XXX: The 1.10 ConfigureWindow definiton has value_mask
             -- explicitly listed in the protocol definition, but everywhere
             -- else it isn't; to keep things uniform, we remove it here.
             ("xproto", "ConfigureWindow") -> nub $ theArgs
             -- XXX: QueryTextExtents has a field named "odd_length" with a
             -- fieldref of "string_len", so we fix it up here to match.
             ("xproto", "QueryTextExtents") -> replace ["odd_length"]
                                                       ["string_len"]
                                                       theArgs
             _ -> theArgs
      packStr = addStructData prefix $ intercalate "" keys
      write = mkCall "buf.write" [mkCall "struct.pack"
                                         (mkStr ('=' : packStr) : (map mkName args'))]
      writeStmt = if length packStr > 0 then [StmtExpr write ()] else []
  in (methodArgs, buf ++ writeStmt ++ lists')

mkPackMethod :: String
             -> String
             -> TypeInfoMap
             -> [GenStructElem Type]
             -> Statement ()
mkPackMethod ext name m structElems =
  let accessor = ((++) "self.")
      (_, packStmts) = mkPackStmts ext name m accessor "" structElems
      ret = [mkReturn $ mkCall "buf.getvalue" noArgs]
  in mkMethod "pack" (mkParams ["self"]) $ packStmts ++ ret

-- | Make a struct style (i.e. not union style) unpack.
mkStructStyleUnpack :: String
                    -> String
                    -> TypeInfoMap
                    -> [GenStructElem Type]
                    -> Suite ()
mkStructStyleUnpack prefix ext m membs =
  let unpackF = structElemToPyUnpack ext m
      (toUnpack, lists) = partitionEithers $ map unpackF membs
      (names, packs) = unzip toUnpack
      packs' = case prefix of
                 "" -> concat packs
                 _ -> addStructData prefix $ concat packs
      names' = catMaybes names
      base = [mkAssign "base" $ mkName "unpacker.offset"]
      assign = mkUnpackFrom names' packs' False
      baseTUnpack = if length names' > 0 then [assign] else []

      bufsize =
        let rhs = BinaryOp (Minus ()) (mkName "unpacker.offset") (mkName "base") ()
        in [mkAssign (mkAttr "bufsize") rhs]

      statements = base ++ baseTUnpack ++ lists ++ bufsize
  in statements

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
  do modify $ mkModify ext name (BaseType "I")
     return Noop
processXDecl _ (XImport n) =
  return $ Declaration [mkImport n]
processXDecl _ (XEnum name membs) =
  return $ Declaration [mkEnum name $ xEnumElemsToPyEnum id membs]
processXDecl ext (XStruct n membs) = do
  m <- get
  let statements = mkStructStyleUnpack "" ext m membs
      pack = mkPackMethod ext n m membs
  modify $ mkModify ext n (CompositeType ext n)
  return $ Declaration [mkXClass n "xcffib.Struct" statements [pack]]
processXDecl ext (XEvent name number membs noSequence) = do
  m <- get
  let cname = name ++ "Event"
      prefix = if fromMaybe False noSequence then "x" else "x{0}2x"
      statements = mkStructStyleUnpack prefix ext m membs
      eventsUpd = mkDictUpdate "_events" number cname
  return $ Declaration [ mkXClass cname "xcffib.Event" statements []
                       , eventsUpd
                       ]
processXDecl ext (XError name number membs) = do
  m <- get
  let cname = name ++ "Error"
      statements = mkStructStyleUnpack "xx2x" ext m membs
      errorsUpd = mkDictUpdate "_errors" number cname
      alias = mkAssign ("Bad" ++ name) (mkName cname)
  return $ Declaration [ mkXClass cname "xcffib.Error" statements []
                       , alias
                       , errorsUpd
                       ]
processXDecl ext (XRequest name number membs reply) = do
  m <- get
  let (args, packStmts) = mkPackStmts ext name m id "x{0}2x" membs
      cookieName = (name ++ "Cookie")
      replyDecl = concat $ maybeToList $ do
        reply' <- reply
        let replyStmts = mkStructStyleUnpack "x{0}2x4x" ext m reply'
            replyName = name ++ "Reply"
            theReply = mkXClass replyName "xcffib.Reply" replyStmts []
            replyType = mkAssign "reply_type" $ mkName replyName
            cookie = mkClass cookieName "xcffib.Cookie" [replyType]
        return [theReply, cookie]

      hasReply = if length replyDecl > 0
                 then [ArgExpr (mkName cookieName) ()]
                 else []
      isChecked = pyTruth $ isJust reply
      argChecked = ArgKeyword (ident "is_checked") (mkName "is_checked") ()
      checkedParam = Param (ident "is_checked") Nothing (Just isChecked) ()
      allArgs = (mkParams $ "self" : args) ++ [checkedParam]
      mkArg = flip ArgExpr ()
      ret = mkReturn $ mkCall "self.send_request" ((map mkArg [ mkInt number
                                                              , mkName "buf"
                                                              ])
                                                              ++ hasReply
                                                              ++ [argChecked])
      requestBody = packStmts ++ [ret]
      request = mkMethod name allArgs requestBody
  return $ Request request replyDecl
processXDecl ext (XUnion name membs) = do
  m <- get
  let unpackF = structElemToPyUnpack ext m
      (fields, lists) = partitionEithers $ map unpackF membs
      toUnpack = map mkUnionUnpack fields
      initMethod = lists ++ toUnpack
      decl = [mkXClass name "xcffib.Union" initMethod []]
  modify $ mkModify ext name (CompositeType ext name)
  return $ Declaration decl
  where
    mkUnionUnpack :: (Maybe String, String)
                  -> Statement ()
    mkUnionUnpack (n, typ) =
      mkUnpackFrom (maybeToList n) typ False

processXDecl ext (XidUnion name _) =
  -- These are always unions of only XIDs.
  do modify $ mkModify ext name (BaseType "I")
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
