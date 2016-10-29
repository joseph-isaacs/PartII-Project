module ParsingAST(module ParsingAST)
  where


data Body = TTopDecls [TopDecl]
  deriving (Show,Eq)

data TopDecls = TTopDecl [TopDecl]
  deriving (Show,Eq)

data TopDecl = TData SimpleType [Constr]
             | TDecl Decl
  deriving (Show,Eq)

type TVar = String
type GTCon = String


data AType = TTyVar TVar
           | TGTyCon GTCon
           | TATypeNested [AType]
           | TATypeArrow AType AType
  deriving (Show,Eq)

type TYCon = String

data SimpleType = TSimpleType TYCon [TVar]
  deriving (Show,Eq)

type ConID = String

data Constr = TConstr ConID [AType]
  deriving (Show,Eq)

type VarID = String

data Decl = TGenDecl [VarID] AType
          | TFunDecl FunLHS  Exp
  deriving (Show,Eq)

data FunLHS = TVarPat Pat [VarID]
  deriving (Show,Eq)


data Exp = TELambda VarID Exp
          | TELet    Decl  Exp
          | TECase   Exp   [Alt]
          | TEApp    Exp   Exp
          | TEVar    VarID
          | TEConstr ConID
          | TELiteral Literal
  deriving (Show,Eq)

data Alt = TAlt Pat Exp
  deriving (Show,Eq)

data Pat = TPat ConID [Pat]
         | TVarID     VarID
         | TLiteral   Literal
  deriving (Show,Eq)

data Literal = TInteger Integer
             | TChar    Char
             | TString  String
  deriving (Show,Eq)

