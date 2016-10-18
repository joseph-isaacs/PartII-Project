{
module Parser (jvhcParse) where

import qualified Lexer as L
import Lexer (LToken)
import AST
}

%name jvhcParse
%tokentype { L.LToken }
%error { parseError }

%token
  '{'    { L.Special  L.LCurly }
  '}'    { L.Special  L.RCurly }
  '('    { L.Special  L.LParen }
  ')'    { L.Special  L.RParen }
  '='    { L.ReservedOP  L.Equal }
  '|'    { L.ReservedOP  L.Pipe  }
  ','    { L.Special  L.Comma }
  '->'   { L.ReservedOP L.RArrow }
  '::'   { L.ReservedOP L.DColon }
  '\\'   { L.ReservedOP L.BSlash }
  ';'    { L.Special L.SemiColon } 
  data   { L.ReserveID  L.Data   }
  tycon  { L.Conid      $$       }
  tyvar  { L.Varid      $$       }
  qtycon { L.Conid      $$       }
  let    { L.ReserveID  L.Let    }
  in     { L.ReserveID  L.In    }
  case   { L.ReserveID  L.Case    }
  of     { L.ReserveID  L.Of    }
  lit    { L.Literal    $$     }

%left '::'
%right '->'

%%

Body :: { Body }
Body : '{' TopDecls '}'      { TTopDecls $2 }

TopDecls :: { [ TopDecl ] }
TopDecls : TopDecl ';' TopDecls  { $1 : $3  }
         | TopDecl               { [$1] }

TopDecl  :: { TopDecl }
TopDecl  : data SimpleType '=' Constrs { TData $2 $ reverse $4 }
         | Decl                        { TDecl $1  }

SimpleType :: { SimpleType }
SimpleType :  tycon TyVars    { TSimpleType $1 $ reverse $2 }

TyVars :: { [TVar] }
TyVars : TyVars tyvar { $2 : $1 }
       |              { [] }


Constrs :: { [Constr] }
Constrs :  Constr '|' Constrs { $1 : $3 }
        |  Constr              { [$1] }

Constr :: { Constr }
Constr : tycon ATypes       { TConstr $1 $2 }

ATypes :: { [AType] }
ATypes : AType ATypes { $1 : $2 }
       |              { []      }

AType :: { AType }
AType : tyvar         { TTyVar $1    }
      | tycon         { TGTyCon $1   }
      | '(' Type ')'  { $2           }


BType :: { [AType] }
BType : BType AType   { $2 : $1 }
      | AType         { [$1]    }

Type :: { AType }
Type : BType             { mkNestedT $1 }
     | BType '->' Type   { TATypeArrow (mkNestedT $1) $3 }


Decl :: { Decl }
Decl :  Gendecl    { (\(v,t) -> TGenDecl v t)$1   }
     |  FunLHS RHS { TFunDecl $1 $2 }

Gendecl :: { ([VarID], AType) }
Gendecl : Vars '::' Type { ($1,$3) }

Vars :: { [VarID] }
Vars : tyvar ',' Vars { $1 : $3}
     | tyvar          { [$1]   }

FunLHS :: { FunLHS }
FunLHS :  tyvar APats { TVarPat (TVarID $1) $2 }
       |  Pat         { TVarPat $1 []                    }

APats :: { [VarID] }
APats : tyvar       { [$1]    }
      | tyvar APats { $1 : $2 }

RHS :: { Exp }
RHS : '=' Exp { $2 }

Exp :: { Exp }
--Exp : ExpC '::' Type { TExpTypeSig $1 $3 }
    Exp : ExpC           { TExp        $1    }

ExpC :: { ExpC }
ExpC : '\\' tyvar '->' Exp       { TELambda $2 $4 }
     | let '{' Decl '}' in Exp   { TELet $3 $6            }
     | case Exp of '{' Alts  '}' { TECase $2 $5 }
     | FExp                      { $1 } 

FExp :: { ExpC }
FExp : AExp       { $1 }
     | AExp FExp  { TEApp (TExp $1) (TExp $2) }

AExp :: { ExpC }
AExp : tyvar                     { TEVar $1               }
     | tycon                     { TEConstr $1 }
     | lit                       { TELiteral $1 }
     | '(' Exp ')'               { TEExp $2 }


Alts :: { [Alt] }
Alts : Alt ';' Alts { $1 : $3 }
     | Alt          { [$1]    }

Alt :: { Alt }
Alt : Pat '->' Exp { TAlt $1 $3 }

Pat :: { Pat }
Pat : tycon APatsWithLiteral { TPat $1 $2 }
    | APatWithLiteral        { $1 }

APatsWithLiteral :: { [Pat] }
APatsWithLiteral : APatWithLiteral APatsWithLiteral { $1 : $2 }
                 |                                  { []      }

APatWithLiteral :: { Pat }
APatWithLiteral : tyvar   { TVarID   $1 }
                | lit     { TLiteral $1 }



{
parseError :: [LToken] -> a
parseError t = error $ (show t) ++  "Parse Error"

mkNestedT :: [AType] -> AType
mkNestedT = TATypeNested . reverse

}
