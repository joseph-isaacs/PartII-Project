module Desugar.DPat where

import CoreAST.DataCon

import Parsing.ParsingAST as PP
import Desugar.DExpr      as DE
import Infer.Assumption
import Desugar.DTypes


-- takes a list of all data constructors and a ParsingAST Pat
dPat :: Monad m => (TypeList,[Assumption]) -> PP.Pat -> m DE.Pat
dPat _         (PP.TLiteral l)    = return $ DE.PLit l
dPat _         (PP.TVarID vid)    = return $ DE.PVar vid
dPat as@(tl,dc) (PP.TPat cid pats) =
  do
    cAssump <- find cid dc
    pats'   <- mapM (dPat as) pats
    return $ DE.PCon (cid :>: cAssump) pats'
