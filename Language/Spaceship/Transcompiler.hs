-- | Source-to-source compiler from a subset of Haskell
--   to vanilla Erlang/OTP code before compiling to BEAM
module Language.Spaceship.Transcompiler where

import Language.Haskell.Exts
import Data.Monoid
import Data.List
import Data.Char (toLower, toUpper, isUpper, isLower)
import System.Environment
import qualified Data.Map as M
import System.Process
import GHC.IO.Exception (ExitCode(..))
import System.FilePath (takeBaseName, dropExtension, dropFileName)
import Data.Maybe

data ParseContext =
   ParseContext { topLevels       :: [String]
                , fArities        :: M.Map String Int
                }
   deriving (Show)

main = do
   [hsFName] <- getArgs
   ExitSuccess <- typecheck hsFName
   main' hsFName

main' hsFName = do
   s <- readFile hsFName
   let eSrc = "-module(" <> (erlModuleName hsFName) <> ").\n" <> (main'' s)
   writeFile erlFName eSrc
   prettyPrinted <- readProcess "./code_transforms/erl_pretty_print" [erlFName] ""
   writeFile erlFName prettyPrinted
   putStrLn $ "Compiled: " <> erlFName
      where
         erlFName =
            dropFileName hsFName
            <> erlModuleName hsFName
            <> ".erl"

erlModuleName :: String -> String
erlModuleName hsFName =
   (upperCamelToSnake . takeBaseName) hsFName

main'' :: String -> String
main'' fSource =
   let ParseOk p = parse fSource
   in (erlangIt . makeECompliant) p

typecheck fName =
   system $
      "ghc -XNoImplicitPrelude -e \"putStrLn \\\"Typechecked\\\"\" "
      <> fName

makeECompliant :: Module -> Module
makeECompliant m@(Module{}) = id m

erlangIt :: Module -> String
erlangIt p@(Module _ _ _ _ _ _ decls) =
   let parseContext =
        ParseContext {
           topLevels = (M.keys builtIns) ++ (topLevelFs p)
          ,fArities  =
             let allOnes =
                  [ arityNum m | (FunBind m) <- decls ]
                  <> [ (n, 0) | (PatBind _ (PVar (Ident n)) _ _ _) <- decls ]
             in M.fromList allOnes
          }
       arityNum :: [Match] -> (String, Int)
       arityNum ((Match _ (Ident name) pats _ _ _):_) =
          (name, length pats)
   in prettyM parseContext p

topLevelFs :: Module -> [String]
topLevelFs (Module _ _ _ _ _ _ decls) =
   ((nub . concat)
           [ [ n' | (Match _ (Ident n') _ _ _ _) <- ms ]
           | (FunBind ms) <- decls 
           ])
   ++ [ n | (PatBind _ (PVar (Ident n)) _ _ _) <- decls ]

prettyM parseContext (Module loc _ _ _ exportSpec _ actualSource) =
   showExports parseContext exportSpec <> "\n\n"
   <> (intercalate "\n" $ map (decl parseContext) actualSource)

showExports parseContext exportSpec =
   case exportSpec of
      Nothing -> "-compile(export_all)."
      Just ([EVar (UnQual (Ident "main"))]) ->
         let erlExports =
              [ camelToSnake n <> "/" <> show arity
              | (n, arity) <- M.toList (fArities parseContext) ]
         in  "-export([" <> intercalate ", " erlExports <> "])."
      Just ds ->
        let fsExported = [ qName q | (EVar q) <- ds ] -- non-total
            erlExports =
               [ (camelToSnake n) <> "/" <> (show $ fArities parseContext M.! n)
               | n <- fsExported ]
        in "-export([" <> intercalate ", " erlExports <> "])."

decl :: ParseContext -> Decl -> String
decl parseContext (FunBind fs) =
   intercalate ";\n" (map (handleMatch parseContext) fs) <> ".\n"
decl parseContext x@(DataDecl _ DataType _ name typeVars decls _) =
   "-type "<> (upperCamelToSnake . nameShape) name <> "("
   <> intercalate ", " (map showTypeVar typeVars) <> ") :: \n" <> space <> "  "
   <> intercalate ("\n" <> space <> "| ") (map eachPiece decls)
   <> "."
decl parseContext (TypeSig _ [name] t) = -- non-total, to error on non-singleton list
   "-spec " <> (upperCamelToSnake . nameShape) name <> "("
   <> walkTypeSig t <> "."
      where
         -- essentially traversing a linked list, where the last
         --   element should be treated differently:
         -- walkTypeSig (TyFun a@(TyCon{}) b@(TyFun{})) =
         walkTypeSig (TyFun a b@(TyFun{})) =
            showInnerDataType' a <> ", " <> walkTypeSig b
         walkTypeSig (TyFun a b) =
            showInnerDataType' a <> ") -> " <>
            showInnerDataType' b
decl parseContext (PatBind _loc name _type rhs _binds) =
   upperCamelToSnake (pat name) <> "() ->\n"
   <> space <> fBody parseContext rhs <> "."
decl parseContext x = show x -- TEMP


decl_in_Do parseContext (PatBind _loc name _type rhs _binds) =
   (pat name) <> " = " <> fBody parseContext rhs


-- toplevel function definitions:
handleMatch parseContext (Match _ name args _type rhs _) =
   (camelToSnake . nameShape $ name) <> "("
   <> intercalate ", " (map pat args) 
   <> ") ->\n" <> space <> fBody parseContext rhs

fBody :: ParseContext -> Rhs -> String
fBody parseContext (UnGuardedRhs exp) =
   expr parseContext exp

expr :: ParseContext -> Exp -> String
expr parseContext (Paren x) = "(" <> expr parseContext x <> ")"
expr parseContext (Lit x) = literal x
expr parseContext (Tuple _ vals) =
   "{" <> intercalate ", " (map (expr parseContext) vals) <> "}"
expr parseContext v@(Var{}) =
   showVar parseContext v
expr parseContext (Do statements) =
   intercalate (",\n" <> space) $
      map (stmt_Do parseContext) statements
expr parseContext (InfixApp a (QConOp (Special Cons)) b) =
   "[ " <> expr parseContext a <> " | " <> expr parseContext b <> " ]"

expr parseContext (InfixApp a (QVarOp (UnQual (Symbol "$"))) b) =
   expr parseContext $ App a b
expr parseContext (InfixApp a op b) =
   fPretty $ fReshape (qShape op) [expr parseContext a, expr parseContext b]
-- App can be a few things:
  -- "toplevel" functions applied
  -- non-"toplevel" fs applied -- lambdas assigned to variables
  -- data constructors: "Foo 3 5"
  -- applying lambda directly to args
expr parseContext app@(App{}) = appHandle parseContext app []
  where
   appHandle :: ParseContext -> Exp -> [Exp] -> String
   appHandle parseContext (App (App a b) c) laterArgs =
      appHandle parseContext (App a b) (c:laterArgs)
   appHandle parseContext (App (Var q) b) laterArgs =
      appHandle' parseContext (qName q) (b:laterArgs)
   appHandle parseContext (App (Con a) b) laterArgs =
      appHandle' parseContext (upperFirst . qName $ a) (b:laterArgs)
   appHandle parseContext (App a b) laterArgs =
      appHandle' parseContext (expr parseContext a) (b:laterArgs)
   appHandle' :: ParseContext -> String -> [Exp] -> String
   appHandle' parseContext name laterArgs =
      if (isUpper . head) name
         then
            let tag = upperCamelToSnake name
                rest = map (expr parseContext) laterArgs
            in fPretty (ETupleShape tag rest)
         else
            if name `elem` (topLevels parseContext)
               then fPretty $ fReshape name (map (expr parseContext) laterArgs)
               else fPretty $ fReshape (upperFirst name) (map (expr parseContext) laterArgs)


expr parseContext (List xs) =
   "[" <> intercalate ", " (map (expr parseContext) xs) <> "]"
expr parseContext (ListComp expression qualStatements) =
   "[ " <> expr parseContext expression <> " || "
   <> intercalate ", " (map (qual_stmt_LC parseContext) qualStatements)
   <> " ]"
expr parseContext (EnumFrom{}) =
   error "this language is strict, so doesn't allow [a..] expressions"
expr parseContext (EnumFromTo from to) =
   "lists:seq(" <> expr parseContext from <> ", " <> expr parseContext to <> ")"
expr parseContext (EnumFromThenTo from andThen to) =
   "lists:seq(" <> expr parseContext from <> ", " <> expr parseContext to
   <> ", " <> expr parseContext andThen <> " - " <> expr parseContext from
   <> ")"
expr parseContext (Con q) = (upperCamelToSnake . qName) q
expr parseContext (Lambda _loc args body) =
   "fun(" <> intercalate ", " (map pat args) <> ") -> "
   <> expr parseContext body <> " end"
expr parseContext (Case e alts) =
   "case " <> expr parseContext e <> " of\n   "
   <> intercalate ";\n   " (map (showAlt parseContext) alts)
   <> "\n   end"
expr parseContext (If pred true false) =
   "case " <> expr parseContext pred <> " of\n" <>
   "   true  -> " <> expr parseContext true  <> ";\n   " <>
   "   false -> " <> expr parseContext false <> "\n" <>
   "end"
expr parseContext x = show x


showVar parseContext (Var v) =
   if qName v `elem` (topLevels parseContext)
      then let fArity = (fArities parseContext) M.! (qName v)
           in if fArity == 0
              then fPretty $ fReshape (qName v) []
              else "fun " <> qName v <> "/" <> show fArity
      else upperFirst $ qName v -- user-defined variable


showAlt :: ParseContext -> Alt -> String
showAlt parseContext (Alt _srcLoc pattern (UnGuardedAlt e) _binds) = -- not total
   pat pattern <> " -> " <> expr parseContext e

qual_stmt_LC :: ParseContext -> QualStmt -> String
qual_stmt_LC parseContext (QualStmt s) = stmt_LC parseContext s
qual_stmt_LC parseContext x = show x

qual_stmt_Do :: ParseContext -> QualStmt -> String
qual_stmt_Do parseContext (QualStmt s) = stmt_Do parseContext s
qual_stmt_Do parseContext x = show x

-- maybe DRY:
data EApplyShape =
    ESpecialShape
      { name :: String, args :: [String] }
  | EInfixShape
      { name :: String, args :: [String] }
  | EPrefixShape
      { name :: String, args :: [String] }
  | ETupleShape
      { name :: String, args :: [String] }
  | EFlipShape
      { name :: String, args :: [String] }
   deriving (Show)

fReshape :: String -> [String] -> EApplyShape
fReshape fName args =
   case M.lookup fName builtIns of
      Just a  -> a args
      Nothing -> EPrefixShape fName args

builtIns = M.fromList $ map (\(a,_,b)->(a,b)) builtInsWArities
builtInsWArities = [
      (":",           2, ESpecialShape "cons")
    , ("&&",          2, EInfixShape "andalso")
    , ("||",          2, EInfixShape "orelse")
    , ("getLine",     0, ESpecialShape "getLine")
    , ("+",           2, EInfixShape "+")
    , ("*",           2, EInfixShape "*")
    , ("-",           2, EInfixShape "-")
    , ("/",           2, EInfixShape "/")
    , ("++",          2, EInfixShape "++")
    , ("\\\\",        2, EInfixShape "--")
    , ("==",          2, EInfixShape "==") -- maybe "=:="
    , ("/=",          2, EInfixShape "/=") -- maybe "=/="
    , (">",           2, EInfixShape ">")
    , ("<",           2, EInfixShape "<")
    , (">=",          2, EInfixShape ">=")
    , ("<=",          2, EInfixShape "=<")
    , ("div",         2, EInfixShape "div")
    , ("rem",         2, EInfixShape "rem")
    , ("print",       1, ESpecialShape "print")
    , ("putStrLn",    1, ESpecialShape "putStrLn")
    , ("putStr",      1, ESpecialShape "putStr")
    , ("return",      1, ESpecialShape "return")
    , ("show",        1, ESpecialShape "show")
    , ("reverse",     1, EPrefixShape "lists:reverse")
    , ("sum",         1, EPrefixShape "lists:sum")
    , ("not",         1, EPrefixShape "not")
    , ("$",           2, ESpecialShape "$")
    , ("map",         2, EPrefixShape "lists:map")
    , ("foldl",       3, EPrefixShape "lists:foldl")
    , ("foldr",       3, EPrefixShape "lists:foldr")
    , ("foldl'",      3, EPrefixShape "lists:foldl")
    , ("head",        1, EPrefixShape "hd")
    , ("length",      1, EPrefixShape "length")
    , ("tail",        1, EPrefixShape "tl")

    -- Language.Spaceship.GbTrees:
    , ("delete",      2, EPrefixShape  "gb_trees:delete_any")
    , ("empty",       0, EPrefixShape  "gb_trees:empty")
    , ("insert",      2, EPrefixShape  "gb_trees:enter")
    , ("!",           2, EFlipShape    "gb_trees:get")
    , ("member",      2, EPrefixShape  "gb_trees:is_defined")
    , ("keys",        1, EPrefixShape  "gb_trees:keys")
    , ("mapWithKey",  2, EPrefixShape  "gb_trees:map")
    , ("size",        1, EPrefixShape  "gb_trees:size")
    , ("toList",      1, EPrefixShape  "gb_trees:to_list")
    , ("elems",       1, EPrefixShape  "gb_trees:values")
    , ("gbTreeNull",  1, EPrefixShape  "gb_trees:is_empty")
    , ("gbTreeFromList", 1, ESpecialShape "gbTreeFromList")
    ]
fPretty :: EApplyShape -> String
fPretty (EInfixShape fName [a, b]) = -- intentionally non-total
   "(" <> a <> " " <> fName <> " " <> b <> ")"
fPretty (EPrefixShape fName args) =
   let nm =
        if isLower (head fName)
        then toSnake fName
        else fName
   in nm <> "(" <> intercalate ", " args <> ")"
fPretty (EFlipShape fName [a, b]) =
   fPretty (EPrefixShape fName [b, a])
fPretty (ESpecialShape "cons" [a,b]) =
   "[ " <> a <> " | " <> b <> " ]"
fPretty (ESpecialShape "print" [a]) =
   "io:format(\"~p~n\", [ " <> a <> " ])"
fPretty (ESpecialShape "putStrLn" [a]) =
   "io:format(\"~s~n\", [ " <> a <> " ])"
fPretty (ESpecialShape "putStr" [a]) =
   "io:format(\"~s\", [ " <> a <> " ])"
fPretty (ESpecialShape "return" [a]) =
   a
fPretty (ESpecialShape "show" [a]) =
   "io_lib:format(\"~p\", [ " <> a <> " ])"
fPretty (ESpecialShape "getLine" []) =
   "io:get_line(\"\")"
fPretty (ESpecialShape "gbTreeFromList" [a]) =
   "gb_trees:from_orddict(orddict:from_list(" <> a <> "))"
fPretty (ETupleShape fName args) =
   "{" <> fName <> ", " <> intercalate ", " args <> "}"
fPretty x = show x -- TEMP

fOrTypeApply a bs =
   if isUpper (head a)
      then fPretty $ ETupleShape (upperCamelToSnake a) bs
      else fPretty $ EPrefixShape a bs

qShape (QVarOp q) = qName q
qShape (QConOp q) = qName q

literal :: Literal -> String
literal (String s)  = show s -- show to preserve the '"'s
literal (Int i)     = show i
literal (Char c)    = "$" <> [c]
literal (Frac r)    = (show . fromRational) r
-- Not covered yet:
{-
  | PrimInt Integer
  | PrimWord Integer
  | PrimFloat Rational
  | PrimDouble Rational
  | PrimChar Char
  | PrimString String
-}



eachPiece (QualConDecl _ _ _ partThatMatters) =
   showDataSum partThatMatters

showDataSum (ConDecl name []) =
   (upperCamelToSnake $ nameShape name)
showDataSum (ConDecl name typesItHolds) =
   "{" <> (upperCamelToSnake . nameShape $ name) <> ", "
   <> intercalate ", " (map  showInnerDataType typesItHolds)
   <> "}"
showDataSum a = show a

showInnerDataType :: BangType -> String
showInnerDataType (UnBangedTy t) = showInnerDataType' t
showInnerDataType (BangedTy   t) = showInnerDataType' t
showInnerDataType (UnpackedTy t) = showInnerDataType' t

showInnerDataType' :: Type -> String
showInnerDataType' (TyCon (UnQual a)) =
   nativeTypeTranslation $ nameShape a
showInnerDataType' (TyList t) =
   "[" <> showInnerDataType' t <> "]"
showInnerDataType' (TyTuple _ elems) =
   "{" <> intercalate ", " (map showInnerDataType' elems) <> "}"
showInnerDataType' (TyVar n) = upperFirst $ nameShape n
showInnerDataType' (TyParen t) = showInnerDataType' t
showInnerDataType' (TyFun a b) = "fun((" <> innerType a b <> ")"
   where
      innerType a (TyFun a' b') = 
         showInnerDataType' a <> ", " <> innerType a' b'
      innerType a b =
         showInnerDataType' a <> ") -> " <>
         showInnerDataType' b
showInnerDataType' (TyApp a b) = tyAppInner a b <> ")"
   where
      tyAppInner (TyApp a' b') b =
         case tyAppInner a' b' of
            -- there should be a collection of these
            --   (erlang doesn't allow the programmer to specify inner types of
            --   e.g. gb_trees):
            "gb_tree(" -> "gb_tree("
            other      -> other <> ", " <> showInnerDataType' b
      tyAppInner (TyCon q) b = 
         case (upperCamelToSnake . qName) q of
            "gb_tree" -> "gb_tree("
            other      -> other <> "(" <> showInnerDataType' b
      tyAppInner a b =
         show a <> showInnerDataType' b
  {-
 TyForall (Maybe [TyVarBind]) Context Type
 TyInfix Type QName Type
 TyKind Type Kind
-}
showInnerDataType' x = show x

showTypeVar :: TyVarBind -> String
showTypeVar (UnkindedVar n) = upperFirst $ nameShape n

stmt_LC :: ParseContext -> Stmt -> String
stmt_LC parseContext (Qualifier e) = expr parseContext e
stmt_LC parseContext (Generator _src_loc p e) =
   pat p <> " <- " <> expr parseContext e

stmt_Do :: ParseContext -> Stmt -> String
stmt_Do parseContext (Qualifier e) = expr parseContext e
stmt_Do parseContext (Generator _src_loc p e) =
   pat p <> " = " <> expr parseContext e
stmt_Do parseContext (LetStmt (BDecls [aDecl])) =
   decl_in_Do parseContext aDecl

nativeTypeTranslation "Integer"  = "integer()"
nativeTypeTranslation "Int"      = "integer()"
nativeTypeTranslation "String"   = "string()"
nativeTypeTranslation "Bool"     = "boolean()"
nativeTypeTranslation "Char"     = "char()"
nativeTypeTranslation "Float"    = "float()"
nativeTypeTranslation "Double"   = "float()" -- not sure about this one
nativeTypeTranslation x = (upperCamelToSnake x) <> "()"




pat :: Pat -> String
pat (PVar name) = upperFirst $ nameShape name
pat (PTuple _ elems) =
   "{" <> intercalate ", " (map pat elems) <> "}"
pat (PApp constructor []) =
   (upperCamelToSnake . qName) constructor
pat (PApp constructor vals) =
   let allVals = (upperCamelToSnake $ qName constructor):(map pat vals)
   in  "{" <> intercalate ", " allVals <> "}"
pat (PParen x) = pat x -- should it ever be "(" <> pat x <> ")"?
pat (PAsPat name val) =
   upperFirst (nameShape name) <> " = " <> pat val
pat PWildCard = "_"
pat (PInfixApp a (Special Cons) b) =
   "[ " <> pat a <> " | " <> pat b <> " ]"
pat (PLit l) = literal l
pat (PBangPat p) = pat p
pat (PList ps) =
   "[ " <> intercalate ", " (map pat ps) <> " ]"

-- missing:
{-
  | PNeg Pat
  | PNPlusK Name Integer
  | PInfixApp Pat QName Pat
  | PRec QName [PatField]
  | PIrrPat Pat
  | PatTypeSig SrcLoc Pat Type
  | PViewPat Exp Pat
  | PRPat [RPat]
  | PXTag SrcLoc XName [PXAttr] (Maybe Pat) [Pat]
  | PXETag SrcLoc XName [PXAttr] (Maybe Pat)
  | PXPcdata String
  | PXPatTag Pat
  | PXRPats [RPat]
  | PExplTypeArg QName Type
  | PQuasiQuote String String
-}
   

nameShape :: Name -> String
nameShape (Ident s) = s
nameShape (Symbol s) = s

qName :: QName -> String
qName (UnQual name) = nameShape name
qName (Qual moduleName name) =
   modName moduleName
   <> ":"
   <> nameShape name
qName (Special UnitCon) = "{}"
qName (Special{}) = error "unmatched"

space = replicate 3 ' ' -- cmd line arg to specify this

modName (ModuleName s) = s

camelToSnake :: String -> String
camelToSnake =
   concatMap foo
      where
         foo '_' = "__"
         foo char =
            if isUpper char
               then ['_', toLower char]
               else [char]

snakeToCamel :: String -> String
snakeToCamel ('_':char:rest) = toUpper char : snakeToCamel rest
snakeToCamel (char:rest) = char : snakeToCamel rest
snakeToCamel [] = []

lowerFirst (a:b) = toLower a : b
lowerFirst [] = []

upperFirst (a:b) = toUpper a : b
upperFirst [] = []

upperCamelToSnake = camelToSnake . lowerFirst

isCamel name =
   (not . null $ intersect name ['A'..'Z'])
   && (not $ '_' `elem` name)

toSnake :: String -> String
toSnake name =
   if isCamel name
      then camelToSnake name
      else name
