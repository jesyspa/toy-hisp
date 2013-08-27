module SkiToC (
    skiToC
) where

-- We use the C++ AST, as it doesn't vary enough to bother changing.
import SKI
import Cpp
import Control.Monad.Free

skiToC :: SKI String r -> CppFile
skiToC = addTopLevel . iter algebra . f
    where f x = x >> return (error "SKI tree incomplete.")

algebra :: SKIRec String CppExpr -> CppExpr
algebra (SKI.Variable x) = AddressOf $ Cpp.Variable x
algebra (SKI.Number x) = Call "mk_num" [Cpp.Number x]
algebra (Combinator x) = AddressOf $ Cpp.Variable $ combName x
algebra (x :@: y) = Call "mk_app" [x, y]

addTopLevel :: CppExpr -> CppFile
addTopLevel tree = File [Include "asm_runtime/main.h", GlobDecl main]
    where main = FunDecl (Pointer $ Simple "object") "make_expr" [] $ Just $ Block [Return tree]



