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

combName :: Combinator -> String
combName S = "comb_s"
combName K = "comb_k"
combName I = "comb_i"
combName L = "comb_l"
combName R = "comb_r"

algebra :: SKI_Rec String CppExpr -> CppExpr
algebra (SKI.Variable x) = Cpp.Variable x
algebra (SKI.Number x) = Cpp.Number x
algebra (Combinator x) = Cpp.Variable $ combName x
algebra (x :@: y) = Call "mk_app" [x, y]

addTopLevel :: CppExpr -> CppFile
addTopLevel tree = File [Include "runtime/main.hpp", Include "runtime/construct.hpp", GlobDecl main]
    where main = FunDecl (Simple "int") "main" [] $ Just $ Block [
                     Block [
                         Decl $ VarDecl Auto "tree" (Just tree),
                         Expr $ Call "eval" [Cpp.Variable "tree"]],
                     Expr $ Call "clear_tmp_roots" [],
                     Expr $ Call "collect_garbage" []]



