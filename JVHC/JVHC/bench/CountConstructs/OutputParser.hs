{-# LANGUAGE TemplateHaskell #-}

module CountConstructs.OutputParser where

import Control.Lens

import Data.String.Utils

data ConstructCount = CC { nLam :: Int, nLet :: Int, nApp :: Int, nVar :: Int, nLit :: Int, nCase :: Int }
  deriving (Eq, Show)

makeLenses ''ConstructCount

emptyCC = CC 0 0 0 0 0 0

addConstructCount :: ConstructCount -> ConstructCount -> ConstructCount
addConstructCount (CC a1 a2 a3 a4 a5 a6) (CC b1 b2 b3 b4 b5 b6) =
  CC (a1+b1) (a2+b2) (a3+b3) (a4+b4) (a5+b5) (a6+b6)

-- Since Let is compiled as Lam in the code gen we
-- double count Lam, once for all Lam expression and then
-- again when the Let is complied as a Lam. This will
-- remove this double count
removeDoubleCountLam :: ConstructCount -> ConstructCount
removeDoubleCountLam cc@(CC { nLam = nlam, nLet = nlet}) =
  cc { nLam = (nlam - nlet)}

funC :: String -> ConstructCount
funC s | startswith "Var"  s = emptyCC { nVar  = 1 }
funC s | startswith "Lit"  s = emptyCC { nLit  = 1 }
funC s | startswith "Lam"  s = emptyCC { nLam  = 1 }
funC s | startswith "Let"  s = emptyCC { nLet  = 1 }
funC s | startswith "App"  s = emptyCC { nApp  = 1 }
funC s | startswith "Case" s = emptyCC { nCase = 1 }
funC _                       = emptyCC

countConstructs :: [String] -> ConstructCount
countConstructs =
  removeDoubleCountLam .
  (foldr (\s acc -> addConstructCount (funC s) acc) emptyCC)
