module Parsing.ParsingAST
  where

import CoreAST.Literal

data Body = TTopDecls [TopDecl]
  deriving (Show,Eq)

data TopDecls = TTopDecl [TopDecl]
  deriving (Show,Eq)

data TopDecl = TData TDataDecl
             | TDecl Decl
  deriving (Show,Eq)

data TDataDecl = TDatadecl SimpleType [Constr]
  deriving (Show,Eq)

type TVar = String
type GTCon = String


data AType = TTyVar TVar
           | TGTyCon GTCon
           | TATypeAp AType AType
           | TATypeArrow AType AType
  deriving (Show,Eq)

type TYCon = String

data SimpleType = TSimpleType TYCon [TVar]
  deriving (Show,Eq)

type ConID = String

data Constr = TConstr ConID [AType]
  deriving (Show,Eq)

type VarID = String

data Decl = TGenDecl TGenDecl
          | TFunDecl TFunDecl
  deriving (Show,Eq)

data TGenDecl = TGendecl [VarID] AType
  deriving (Show,Eq)

data TSGenDecl = TSGendecl VarID AType
  deriving (Show,Eq)

data TFunDecl = TFundecl FunLHS Exp
  deriving (Show,Eq)

data FunLHS = TVarPat VarID [VarID]
  deriving (Show,Eq)


data Exp = TEVar    VarID
         | TELiteral Literal
         | TEConstr ConID
         | TEApp    Exp   Exp
         | TELambda VarID Exp
         | TELet    Decl  Exp
         | TECase   Exp   [Alt]
  deriving (Show,Eq)

data Alt = TAlt Pat Exp
  deriving (Show,Eq)

data Pat = TPat ConID [Pat]
         | TVarID     VarID
         | TLiteral   Literal
  deriving (Show,Eq)

