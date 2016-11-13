module Desugar.DPat where

import Parsing.ParsingAST as PP
import Infer.TIPat        as TIP
import Infer.Assumption
import Desugar.DTypes


-- takes a list of all data constructors and a ParsingAST Pat
dPat :: Monad m => (TypeList,[Assumption]) -> PP.Pat -> m TIP.Pat
dPat _         (PP.TLiteral l)    = return $ TIP.PLit l
dPat _         (PP.TVarID vid)    = return $ TIP.PVar vid
dPat as@(_,dc) (PP.TPat cid pats) =
  do
    cAssump <- find cid dc
    pats'   <- mapM (dPat as) pats
    return $ TIP.PCon (cid :>: cAssump) pats'
