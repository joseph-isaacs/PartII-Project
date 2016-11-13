module Desugar.Pat where

import qualified Parsing.ParsingAST as P

import qualified Infer.TIPat as TIP

dsPat :: P.Pat -> TIP.Pat
dsPat (P.TPat cid pats) = TPat () (map dsPat pats)
dsPat (P.TVarID vID)    = PVar vID
dsPat (P.TLiteral l)    = PLit l
