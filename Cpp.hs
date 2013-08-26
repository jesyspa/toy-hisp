module Cpp (
    CppFile(..),
    CppGlobal(..),
    CppStmt(..),
    CppStmts(..),
    CppExpr(..),
    CppExprs(..),
    CppType(..)
) where

import Generics.MultiRec.Base
import Generics.MultiRec.TH


type CppExprs = [CppExpr]
type CppStmts = [CppStmt]

data CppFile = File [CppGlobal]
    deriving(Show, Read, Eq, Ord)

data CppGlobal
    = Include String
    | GlobDecl CppDecl
    deriving(Show, Read, Eq, Ord)

data CppStmt
    = Block CppStmts
    | Expr CppExpr
    | Decl CppDecl
    deriving(Show, Read, Eq, Ord)

data CppDecl
    = FunDecl CppType String [(CppType, String)] (Maybe CppStmt)
    | VarDecl CppType String (Maybe CppExpr)
    deriving(Show, Read, Eq, Ord)

data CppExpr
    = Call String CppExprs
    | Variable String
    | Number Int
    deriving(Show, Read, Eq, Ord)

data CppType
    = Simple String
    | Auto
    | Pointer CppType
    deriving(Show, Read, Eq, Ord)

data CppAST :: * -> * where
    CppFile   :: CppAST CppFile
    CppGlobal :: CppAST CppGlobal
    CppStmt   :: CppAST CppStmt
    CppDecl   :: CppAST CppDecl
    CppExpr   :: CppAST CppExpr
    CppType   :: CppAST CppType

deriveAll ''CppAST
