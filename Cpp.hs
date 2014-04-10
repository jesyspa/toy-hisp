module Cpp (
    CppFile(..),
    CppGlobal(..),
    CppStmt(..),
    CppDecl(..),
    CppExpr(..),
    CppType(..)
) where

import Generics.MultiRec.TH
import Generics.MultiRec.FoldAlgK as K
import Text.PrettyPrint.Leijen
import Data.Maybe

data CppFile = File [CppGlobal]
    deriving(Show, Read, Eq, Ord)

data CppGlobal
    = Include String
    | GlobDecl CppDecl
    deriving(Show, Read, Eq, Ord)

data CppStmt
    = Block [CppStmt]
    | Expr CppExpr
    | Decl CppDecl
    | Return CppExpr
    deriving(Show, Read, Eq, Ord)

data CppDecl
    = FunDecl CppType String [CppArgDecl] (Maybe CppStmt)
    | VarDecl CppType String (Maybe CppExpr)
    deriving(Show, Read, Eq, Ord)

data CppExpr
    = Call String [CppExpr]
    | Variable String
    | Number Int
    | AddressOf CppExpr
    deriving(Show, Read, Eq, Ord)

data CppType
    = Simple String
    | Auto
    | Pointer CppType
    deriving(Show, Read, Eq, Ord)

data CppArgDecl
    = ArgDecl CppType String
    deriving(Show, Read, Eq, Ord)

data CppAST :: * -> * where
    CppFile    :: CppAST CppFile
    CppGlobal  :: CppAST CppGlobal
    CppStmt    :: CppAST CppStmt
    CppDecl    :: CppAST CppDecl
    CppExpr    :: CppAST CppExpr
    CppType    :: CppAST CppType
    CppArgDecl :: CppAST CppArgDecl

deriveAll ''CppAST

pprintAlgebra :: Algebra CppAST Doc
pprintAlgebra _ = file & (include & globdecl) & (block & expr & decl & return) &
                  (fundecl & vardecl) & (call & variable & number & addressOf) &
                  (simple & auto & pointer) & argdecl
    where file = vsep
          include x = text $ "#include \"" ++ x ++ "\""
          globdecl = id
          block xs = char '{' <$> indent 4 (vsep xs) <$> char '}'
          expr x = x <> char ';'
          decl = id
          return x = text "return" <+> x <> char ';'
          fundecl tp nm args body = tp <+> text nm <+> tupled args <> fromMaybe (char ';') body
          vardecl tp nm Nothing = tp <+> text nm <> char ';'
          vardecl tp nm (Just x)= tp <+> text nm <+> char '=' <+> x <> char ';'
          call nm exprs = text nm <> tupled exprs
          variable = text
          number = int
          addressOf x = char '&' <> x
          simple = text
          auto = text "auto"
          pointer x = x <> char '*'
          argdecl x y = x <+> text y


instance Pretty CppFile where
    pretty = fold pprintAlgebra CppFile
