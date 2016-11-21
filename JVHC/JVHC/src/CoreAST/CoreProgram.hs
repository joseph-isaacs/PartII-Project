module CoreAST.CoreProgram where


data Program = MkProgram { exprDefs :: CoreExprDefs, dConstructor :: DataCon, tConstructor :: TyCon }
