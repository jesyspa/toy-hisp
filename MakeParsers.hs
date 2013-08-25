{-# LANGUAGE TemplateHaskell #-}
module MakeParsers where

import Language.Haskell.TH
import Control.Applicative
import Text.Parsec
import Debug.Trace
import Control.Monad.Plus

wrapAsVal :: Name -> Exp -> Dec
wrapAsVal x e = FunD x [Clause [] (NormalB e) []]

arity :: Con -> Int
arity (NormalC _ xs) = length xs
arity (RecC _ xs) = length xs
arity (InfixC _ _ _) = 2
arity (ForallC _ _ x) = arity x


name :: Con -> Name
name (NormalC x _) = x
name (RecC x _) = x
name (InfixC _ x _) = x
name (ForallC _ _ x) = name x


mkL :: Con -> Q Dec
mkL con = do
    let args = fmap (\x -> mkName $ "arg" ++ show x) [1..arity con]
        cName = name con
        pName = mkName $ "l" ++ nameBase cName
        conP = ConP cName $ fmap VarP args
        val = return . TupE $ fmap VarE args
        eName = stringE $ pprint cName
    eVal <- [| Just $val |]
    eFail <- [| Nothing |]
    let x = mkName "x" -- temp for case
        mVal = Match conP (NormalB eVal) []
        mFail = Match WildP (NormalB eFail) []
        eLam = return $ LamE [VarP x] $ VarE x `CaseE` [mVal, mFail]

    expr <- ([| try $ mmapMaybe $eLam anyToken |])
    return $ wrapAsVal pName expr

mkLs :: Name -> Q [Dec]
mkLs name = do
    TyConI (DataD _ _ _ xs _) <- reify name
    mapM mkL xs
