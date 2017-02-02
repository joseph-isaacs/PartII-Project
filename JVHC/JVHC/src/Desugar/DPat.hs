module Desugar.DPat where

import CoreAST.DataCon

import Parsing.ParsingAST as PP
import Desugar.DExpr      as DE
import Infer.Assumption
import Desugar.DTypes

import qualified Data.List as DL

-- takes a list of all data constructors and a ParsingAST Pat
dPat :: Monad m => [DataType] -> (TypeList,[Assumption]) -> PP.Pat -> m DE.Pat
dPat _ _         (PP.TLiteral l)    = return $ DE.PLit l
dPat _ _         (PP.TVarID vid)    = return $ DE.PVar vid
dPat dt as@(tl,dc) (PP.TPat cid pats) =
  do
    cAssump <- find cid dc
    pats'   <- mapM (dPat dt as) pats
    d <- findDT cid dt
    return $ DE.PCon d (cid :>: cAssump) pats'

findDT :: Monad m => String -> [DataType] -> m DataCon
findDT f dts = case DL.find (\x -> dName x == f) dCons of
              Just x -> return x
              Nothing -> fail $ "Cannot find value in " ++ show f ++ " in "  ++ (show tyCons)
  where tyCons = map tCon dts
        dCons  = concatMap constrs tyCons
