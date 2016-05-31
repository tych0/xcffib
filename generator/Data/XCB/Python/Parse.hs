{-
 - Copyright 2014 Tycho Andersen
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at
 -
 -   http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License.
 -}
{-# LANGUAGE ViewPatterns #-}
module Data.XCB.Python.Parse (
  parseXHeaders,
  xform,
  renderPy,
  calcsize
  ) where

import Control.Applicative hiding (getConst)
import Control.Monad.State.Strict

import Data.Attoparsec.ByteString.Char8
import Data.Bits
import qualified Data.ByteString.Char8 as BS
import Data.Either
import Data.List
import qualified Data.Map as M
import Data.Tree
import Data.Maybe
import Data.XCB.FromXML
import Data.XCB.Types as X
import Data.XCB.Python.PyHelpers

import Language.Python.Common as P

import System.FilePath
import System.FilePath.Glob

import Text.Printf

data TypeInfo =
  -- | A "base" X type, i.e. one described in baseTypeInfo; first arg is the
  -- struct.unpack string, second is the size.
  BaseType String |
  -- | A composite type, i.e. a Struct or Union created by XCB. First arg is
  -- the extension that defined it, second is the name of the type.
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

parseXHeaders :: FilePath -> IO [XHeader]
parseXHeaders fp = do
  files <- namesMatching $ fp </> "*.xml"
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
      let imports = [mkImport "xcffib", mkImport "struct", mkImport "io"]
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

-- | Clone of python's struct.calcsize.
calcsize :: String -> Int
calcsize str = sum [fromMaybe 1 i * getSize c | (i, c) <- parseMembers str]
  where
    sizeM :: M.Map Char Int
    sizeM = M.fromList [ ('c', 1)
                       , ('B', 1)
                       , ('b', 1)
                       , ('H', 2)
                       , ('h', 2)
                       , ('I', 4)
                       , ('i', 4)
                       , ('Q', 8)
                       , ('q', 8)
                       , ('f', 4)
                       , ('d', 8)
                       , ('x', 1)
                       ]
    getSize = (M.!) sizeM

    parseMembers :: String -> [(Maybe Int, Char)]
    parseMembers s = case parseOnly lang (BS.pack s) of
                       Left err -> error ("can't calcsize " ++ s ++ " " ++ err)
                       Right xs -> xs

    lang = many $ (,) <$> optional decimal <*> (satisfy $ inClass $ M.keys sizeM)

xBinopToPyOp :: X.Binop -> P.Op ()
xBinopToPyOp X.Add = P.Plus ()
xBinopToPyOp X.Sub = P.Minus ()
xBinopToPyOp X.Mult = P.Multiply ()
xBinopToPyOp X.Div = P.FloorDivide ()
xBinopToPyOp X.And = P.BinaryAnd ()
xBinopToPyOp X.RShift = P.ShiftRight ()

xUnopToPyOp :: X.Unop -> P.Op ()
xUnopToPyOp X.Complement = P.Invert ()

xExpressionToNestedPyExpr :: (String -> String) -> XExpression -> Expr ()
xExpressionToNestedPyExpr acc (Op o e1 e2) =
  Paren (xExpressionToPyExpr acc (Op o e1 e2)) ()
xExpressionToNestedPyExpr acc xexpr =
  xExpressionToPyExpr acc xexpr

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
      e1' = xExpressionToNestedPyExpr acc e1
      e2' = xExpressionToNestedPyExpr acc e2
  in BinaryOp o' e1' e2' ()
xExpressionToPyExpr acc (Unop o e) =
  let o' = xUnopToPyOp o
      e' = xExpressionToNestedPyExpr acc e
  in Paren (UnaryOp o' e' ()) ()

getConst :: XExpression -> Maybe Int
getConst (Value i) = Just i
getConst (Bit i) = Just $ bit i
getConst (Op o e1 e2) = do
  c1 <- getConst e1
  c2 <- getConst e2
  return $ case o of
             X.Add -> c1 + c2
             X.Sub -> c1 - c2
             X.Mult -> c1 * c2
             X.Div -> c1 `quot` c2
             X.And -> c1 .&. c2
             X.RShift -> c1 `shift` c2
getConst (Unop o e) = do
  c <- getConst e
  return $ case o of
             X.Complement -> complement c
getConst (PopCount e) = fmap popCount $ getConst e
getConst _ = Nothing

xEnumElemsToPyEnum :: (String -> String) -> [XEnumElem] -> [(String, Expr ())]
xEnumElemsToPyEnum accessor membs = reverse $ conv membs [] [0..]
  where
    exprConv = xExpressionToPyExpr accessor
    conv :: [XEnumElem] -> [(String, Expr ())] -> [Int] -> [(String, Expr ())]
    conv ((EnumElem name expr) : els) acc is =
      let expr' = fromMaybe (mkInt (head is)) $ fmap exprConv expr
          is' = dropWhile (<= (fromIntegral (int_value expr'))) is
          acc' = (name, expr') : acc
      in conv els acc' is'
    conv [] acc _ = acc

-- Add the xcb_generic_{request,reply}_t structure data to the beginning of a
-- pack string. This is a little weird because both structs contain a one byte
-- pad which isn't at the end. If the first element of the request or reply is
-- a byte long, it takes that spot instead, and there is one less offset
addStructData :: String -> String -> String
addStructData prefix (c : cs) | c `elem` "Bbx" =
  let result = maybePrintChar prefix c
  in if result == prefix then result ++ (c : cs) else result ++ cs
addStructData prefix s = (maybePrintChar prefix 'x') ++ s

maybePrintChar :: String -> Char -> String
maybePrintChar s c | "%c" `isInfixOf` s = printf s c
maybePrintChar s _ = s

-- Don't prefix a single pad byte with a '1'. This is simpler to parse
-- visually, and also simplifies addStructData above.
mkPad :: Int -> String
mkPad 1 = "x"
mkPad i = (show i) ++ "x"

structElemToPyUnpack :: Expr ()
                     -> String
                     -> TypeInfoMap
                     -> GenStructElem Type
                     -> Either (Maybe String, String)
                               (String, Expr (), Expr (), Maybe Int)
structElemToPyUnpack _ _ _ (Pad i) = Left (Nothing, mkPad i)

-- XXX: This is a cheap hack for noop, we should really do better.
structElemToPyUnpack _ _ _ (Doc _ _ _) = Left (Nothing, "")
-- XXX: What does fd/switch mean? we should implement it correctly
structElemToPyUnpack _ _ _ (Fd _) = Left (Nothing, "")
structElemToPyUnpack _ _ _ (Switch _ _ _) = Left (Nothing, "")

-- The enum field is mostly for user information, so we ignore it.
structElemToPyUnpack unpacker ext m (X.List n typ len _) =
  let attr = ((++) "self.")
      len' = fromMaybe pyNone $ fmap (xExpressionToPyExpr attr) len
      cons = case m M.! typ of
               BaseType c -> mkStr c
               CompositeType tExt c | ext /= tExt -> mkName $ tExt ++ "." ++ c
               CompositeType _ c -> mkName c
      list = mkCall "xcffib.List" [ unpacker
                                  , cons
                                  , len'
                                  ]
      constLen = do
        l <- len
        getConst l
  in Right (n, list, cons, constLen)

-- The mask and enum fields are for user information, we can ignore them here.
structElemToPyUnpack unpacker ext m (SField n typ _ _) =
  case m M.! typ of
    BaseType c -> Left (Just n, c)
    CompositeType tExt c ->
      let c' = if tExt == ext then c else tExt ++ "." ++ c
          field = mkCall c' [unpacker]
      -- TODO: Ugh. Nothing here is wrong. Do we really need to carry the
      -- length of these things around?
      in Right (n, field, mkName c', Nothing)
structElemToPyUnpack _ _ _ (ExprField _ _ _) = error "Only valid for requests"
structElemToPyUnpack _ _ _ (ValueParam _ _ _ _) = error "Only valid for requests"

structElemToPyPack :: String
                   -> TypeInfoMap
                   -> (String -> String)
                   -> GenStructElem Type
                   -> Either (Maybe String, String) [(String, Maybe (Expr ()))]
structElemToPyPack _ _ _ (Pad i) = Left (Nothing, mkPad i)
-- TODO: implement doc, switch, and fd?
structElemToPyPack _ _ _ (Doc _ _ _) = Left (Nothing, "")
structElemToPyPack _ _ _ (Switch _ _ _) = Left (Nothing, "")
structElemToPyPack _ _ _ (Fd _) = Left (Nothing, "")
structElemToPyPack _ m accessor (SField n typ _ _) =
  let name = accessor n
  in case m M.! typ of
       BaseType c -> Left (Just name, c)
       CompositeType _ typNam ->
         let cond = mkCall "hasattr" [mkArg name, ArgExpr (mkStr "pack") ()]
             trueB = mkCall (name ++ ".pack") noArgs
             synthetic = mkCall (typNam ++ ".synthetic") [mkArg ("*" ++ name)]
             falseB = mkCall (mkDot synthetic "pack") noArgs
         in Right $ [(name, Just (CondExpr trueB cond falseB ()))]
-- TODO: assert values are in enum?
structElemToPyPack ext m accessor (X.List n typ expr _) =
  let name = accessor n
      -- The convention seems to be either to have a <fieldref> nested in the
      -- list, or use "%s_len" % name if there is no fieldref. We need to add
      -- the _len to the arguments of the function but we don't need to pack
      -- anything, which we denote using Nothing
      list_len = if isNothing expr then [(name ++ "_len", Nothing)] else []
      list = case m M.! typ of
        BaseType c -> [(name, Just (mkCall "xcffib.pack_list" [ mkName $ name
                                                        , mkStr c
                                                        ]))]
        CompositeType tExt c ->
          let c' = if tExt == ext then c else (tExt ++ "." ++ c)
          in [(name, Just (mkCall "xcffib.pack_list" ([ mkName $ name
                                                , mkName c'
                                                ])))]
  in Right $ list_len ++ list
structElemToPyPack _ m accessor (ExprField name typ expr) =
  let e = (xExpressionToPyExpr accessor) expr
      name' = accessor name
  in case m M.! typ of
       BaseType c -> Right $ [(name', Just (mkCall "struct.pack" [ mkStr ('=' : c)
                                                           , e
                                                           ]))]
       CompositeType _ _ -> Right $ [(name',
                                      Just (mkCall (mkDot e "pack") noArgs))]

-- As near as I can tell here the padding param is unused.
structElemToPyPack _ m accessor (ValueParam typ mask _ list) =
  case m M.! typ of
    BaseType c ->
      let mask' = mkCall "struct.pack" [mkStr ('=' : c), mkName $ accessor mask]
          list' = mkCall "xcffib.pack_list" [ mkName $ accessor list
                                            , mkStr "I"
                                            ]
      in Right $ [(mask, Just mask'), (list, Just list')]
    CompositeType _ _ -> error (
      "ValueParams other than CARD{16,32} not allowed.")

buf :: Suite ()
buf = [mkAssign "buf" (mkCall "io.BytesIO" noArgs)]

mkPackStmts :: String
            -> String
            -> TypeInfoMap
            -> (String -> String)
            -> String
            -> [GenStructElem Type]
            -> ([String], Suite ())
mkPackStmts ext name m accessor prefix membs =
  let packF = structElemToPyPack ext m accessor
      (toPack, stmts) = partitionEithers $ map packF membs
      listWrites = map (flip StmtExpr () . mkCall "buf.write" . (: [])) $ catMaybes lists
      (args, keys) = let (as, ks) = unzip toPack in (catMaybes as, ks)

      -- In some cases (e.g. xproto.ConfigureWindow) there is padding after
      -- value_mask. The way the xml specification deals with this is by
      -- specifying value_mask in both the regular pack location as well as
      -- implying it implicitly. Thus, we want to make sure that if we've already
      -- been told to pack something explcitly, that we don't also pack it
      -- implicitly.
      (listNames, lists) = unzip $ filter (flip notElem args . fst) (concat stmts)
      listNames' = case (ext, name) of
                     -- XXX: QueryTextExtents has a field named "odd_length"
                     -- which is unused, let's just drop it.
                     ("xproto", "QueryTextExtents") ->
                       let notOdd "odd_length" = False
                           notOdd _ = True
                       in filter notOdd listNames
                     _ -> listNames
      packStr = addStructData prefix $ intercalate "" keys
      write = mkCall "buf.write" [mkCall "struct.pack"
                                         (mkStr ('=' : packStr) : (map mkName args))]
      writeStmt = if length packStr > 0 then [StmtExpr write ()] else []
  in (args ++ listNames', writeStmt ++ listWrites)

mkPackMethod :: String
             -> String
             -> TypeInfoMap
             -> Maybe (String, Int)
             -> [GenStructElem Type]
             -> Maybe Int
             -> Statement ()
mkPackMethod ext name m prefixAndOp structElems minLen =
  let accessor = ((++) "self.")
      (prefix, op) = case prefixAndOp of
                        Just ('x' : rest, i) ->
                          let packOpcode = mkCall "struct.pack" [mkStr "=B", mkInt i]
                              write = mkCall "buf.write" [packOpcode]
                          in (rest, [StmtExpr write ()])
                        Just (rest, _) -> error ("internal API error: " ++ show rest)
                        Nothing -> ("", [])
      (_, packStmts) = mkPackStmts ext name m accessor prefix structElems
      extend = concat $ do
        len <- maybeToList minLen
        let bufLen = mkName "buf_len"
            bufLenAssign = mkAssign bufLen $ mkCall "len" [mkCall "buf.getvalue" noArgs]
            test = (BinaryOp (LessThan ()) bufLen (mkInt len)) ()
            bufWriteLen = Paren (BinaryOp (Minus ()) (mkInt 32) bufLen ()) ()
            extra = mkCall "struct.pack" [repeatStr "x" bufWriteLen]
            writeExtra = [StmtExpr (mkCall "buf.write" [extra]) ()]
        return $ [bufLenAssign, mkIf test writeExtra]
      ret = [mkReturn $ mkCall "buf.getvalue" noArgs]
  in mkMethod "pack" (mkParams ["self"]) $ buf ++ op ++ packStmts ++ extend ++ ret

data StructUnpackState = StructUnpackState {
  -- | stNeedsPad is whether or not a type_pad() is needed. As near
  -- as I can tell the conditions are:
  --    1. a list was unpacked
  --    2. a struct was unpacked
  -- ListFontsWithInfoReply is an example of a struct which has lots of
  -- this type of thing.
  stNeedsPad :: Bool,

  -- The list of names the struct.pack accumulator has, and the
  stNames :: [String],

  -- The list of pack directives (potentially with a "%c" in it for
  -- the prefix byte).
  stPacks :: String
}

-- | Make a struct style (i.e. not union style) unpack.
mkStructStyleUnpack :: String
                    -> String
                    -> TypeInfoMap
                    -> [GenStructElem Type]
                    -> (Suite (), Maybe Int)
mkStructStyleUnpack prefix ext m membs =
  let unpacked = map (structElemToPyUnpack (mkName "unpacker") ext m) membs
      initial = StructUnpackState False [] prefix
      (_, unpackStmts, size) = evalState (mkUnpackStmtsR unpacked) initial
      base = [mkAssign "base" $ mkName "unpacker.offset"]
      bufsize =
        let rhs = BinaryOp (Minus ()) (mkName "unpacker.offset") (mkName "base") ()
        in [mkAssign (mkAttr "bufsize") rhs]
      statements = base ++ unpackStmts ++ bufsize
  in (statements, size)

    where

      -- Apparently you only type_pad before unpacking Structs or Lists, never
      -- base types.
      mkUnpackStmtsR :: [Either (Maybe String, String)
                                (String, Expr (), Expr (), Maybe Int)]
                     -> State StructUnpackState ([String], Suite (), Maybe Int)

      mkUnpackStmtsR [] = flushAcc

      mkUnpackStmtsR (Left (name, pack) : xs) = do
        st <- get
        let packs = if "%c" `isInfixOf` (stPacks st)
                    then addStructData (stPacks st) pack
                    else (stPacks st) ++ pack
        put $ st { stNames = stNames st ++ maybeToList name
                 , stPacks = packs
                 }
        mkUnpackStmtsR xs

      mkUnpackStmtsR (Right (listName, list, cons, listSz) : xs) = do
        (packNames, packStmt, packSz) <- flushAcc
        st <- get
        put $ st { stNeedsPad = True }
        let pad = if stNeedsPad st
                  then [typePad cons]
                  else []
        (restNames, restStmts, restSz) <- mkUnpackStmtsR xs
        let totalSize = do
                          before <- packSz
                          rest <- restSz
                          listSz' <- listSz
                          return $ before + rest + listSz'
            listStmt = mkAssign (mkAttr listName) list
        return ( packNames ++ [listName] ++ restNames
               , packStmt ++ pad ++ listStmt : restStmts
               , totalSize
               )

      flushAcc :: State StructUnpackState ([String], Suite (), Maybe Int)
      flushAcc = do
        StructUnpackState needsPad args keys <- get
        let size = calcsize keys
            assign = mkUnpackFrom "unpacker" args keys
        put $ StructUnpackState needsPad [] ""
        return (args, assign, Just size)

      typePad e = StmtExpr (mkCall "unpacker.pad" [e]) ()

-- | Given a (qualified) type name and a target type, generate a TypeInfoMap
-- updater.
mkModify :: String -> String -> TypeInfo -> TypeInfoMap -> TypeInfoMap
mkModify ext name ti m =
  let m' = M.fromList [ (UnQualType name, ti)
                      , (QualType ext name, ti)
                      ]
  in M.union m m'

mkSyntheticMethod :: [GenStructElem Type] -> [Statement ()]
mkSyntheticMethod membs = do
  let names = catMaybes $ map getName membs
      args = mkParams $ "cls" : names
      self = mkAssign "self" $ mkCall (mkDot "cls" "__new__") [mkName "cls"]
      body = map assign names
      ret = mkReturn $ mkName "self"
      synthetic = mkMethod "synthetic" args $ (self : body) ++ [ret]
      classmethod = Decorator [ident "classmethod"] noArgs ()
  if null names then [] else [Decorated [classmethod] synthetic ()]
    where
      getName :: GenStructElem Type -> Maybe String
      getName (Pad _) = Nothing
      getName (X.List n _ _ _) = Just n
      getName (SField n _ _ _) = Just n
      getName (ExprField n _ _) = Just n
      getName (ValueParam _ n _ _) = Just n
      getName (Switch n _ _) = Just n
      getName (Doc _ _ _) = Nothing
      getName (Fd n) = Just n

      assign :: String -> Statement ()
      assign n = mkAssign (mkDot "self" n) $ mkName n

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
  return $ Declaration [ mkRelImport n]
processXDecl _ (XEnum name membs) =
  return $ Declaration [mkEnum name $ xEnumElemsToPyEnum id membs]
processXDecl ext (XStruct n membs) = do
  m <- get
  let (statements, len) = mkStructStyleUnpack "" ext m membs
      pack = mkPackMethod ext n m Nothing membs Nothing
      synthetic = mkSyntheticMethod membs
      fixedLength = maybeToList $ do
        theLen <- len
        let rhs = mkInt theLen
        return $ mkAssign "fixed_size" rhs
  modify $ mkModify ext n (CompositeType ext n)
  return $ Declaration [mkXClass n "xcffib.Struct" statements (pack : fixedLength ++ synthetic)]
processXDecl ext (XEvent name opcode membs noSequence) = do
  m <- get
  let cname = name ++ "Event"
      prefix = if fromMaybe False noSequence then "x" else "x%c2x"
      pack = mkPackMethod ext name m (Just (prefix, opcode)) membs (Just 32)
      synthetic = mkSyntheticMethod membs
      (statements, _) = mkStructStyleUnpack prefix ext m membs
      eventsUpd = mkDictUpdate "_events" opcode cname
  return $ Declaration [ mkXClass cname "xcffib.Event" statements (pack : synthetic)
                       , eventsUpd
                       ]
processXDecl ext (XError name opcode membs) = do
  m <- get
  let cname = name ++ "Error"
      prefix = "xx2x"
      pack = mkPackMethod ext name m (Just (prefix, opcode)) membs Nothing
      (statements, _) = mkStructStyleUnpack prefix ext m membs
      errorsUpd = mkDictUpdate "_errors" opcode cname
      alias = mkAssign ("Bad" ++ name) (mkName cname)
  return $ Declaration [ mkXClass cname "xcffib.Error" statements [pack]
                       , alias
                       , errorsUpd
                       ]
processXDecl ext (XRequest name opcode membs reply) = do
  m <- get
  let
      -- xtest doesn't seem to use the same packing strategy as everyone else,
      -- but there is no clear indication in the XML as to why that is. yay.
      prefix = if ext /= "xproto" then "xx2x" else "x%c2x"
      (args, packStmts) = mkPackStmts ext name m id prefix membs
      cookieName = (name ++ "Cookie")
      replyDecl = concat $ maybeToList $ do
        reply' <- reply
        let (replyStmts, _) = mkStructStyleUnpack "x%c2x4x" ext m reply'
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
      mkArg' = flip ArgExpr ()
      ret = mkReturn $ mkCall "self.send_request" ((map mkArg' [ mkInt opcode
                                                               , mkName "buf"
                                                               ])
                                                               ++ hasReply
                                                               ++ [argChecked])
      requestBody = buf ++ packStmts ++ [ret]
      request = mkMethod name allArgs requestBody
  return $ Request request replyDecl
processXDecl ext (XUnion name membs) = do
  m <- get
  let unpackF = structElemToPyUnpack unpackerCopy ext m
      (fields, listInfo) = partitionEithers $ map unpackF membs
      toUnpack = concat $ map mkUnionUnpack fields
      (names, exprs, _, _) = unzip4 listInfo
      lists = map (uncurry mkAssign) $ zip (map mkAttr names) exprs
      initMethod = lists ++ toUnpack
      -- Here, we only want to pack the first member of the union, since every
      -- member is the same data and we don't want to repeatedly pack it.
      pack = mkPackMethod ext name m Nothing [head membs] Nothing
      decl = [mkXClass name "xcffib.Union" initMethod [pack]]
  modify $ mkModify ext name (CompositeType ext name)
  return $ Declaration decl
  where
    unpackerCopy = mkCall "unpacker.copy" noArgs
    mkUnionUnpack :: (Maybe String, String)
                  -> Suite ()
    mkUnionUnpack (n, typ) =
      mkUnpackFrom unpackerCopy (maybeToList n) typ

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
