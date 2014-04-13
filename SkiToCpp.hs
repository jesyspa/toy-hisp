module SkiToCpp (
    skiToCpp
) where

import SKI
import Cpp
import Control.Monad.Free

skiToCpp :: SKI String r -> CppFile
skiToCpp = addTopLevel . iter algebra . f
    -- if this is ever encountered, it is a bug on our part
    where f x = x >> return (error "SKI tree incomplete.")

algebra :: SKIRec String CppExpr -> CppExpr
algebra (SKI.Variable x) = Cpp.Variable x
algebra (SKI.Number x) = Cpp.Number x
algebra (Combinator x) = Cpp.Variable $ combName x
algebra (x :@: y) = Call "mk_app" [x, y]

addTopLevel :: CppExpr -> CppFile
addTopLevel tree = File [Include "runtime/main.hpp", Include "runtime/construct.hpp", GlobDecl main]
    where main = FunDecl (Simple "int") "main" [] $ Just $ Block [
                     Block [
                         Decl $ VarDecl Auto "s" (Just $ Call "construct" [tree]),
                         Expr $ Call "eval" [Cpp.Variable "s"]],
                     Expr $ Call "collect_garbage" []]

