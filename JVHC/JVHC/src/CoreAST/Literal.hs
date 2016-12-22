module CoreAST.Literal where


data Literal = LitInt     Integer
             | LitChar    Char
             | LitString  String
  deriving (Show,Eq)

