{
module Lexer (alexScanTokens, LToken(..), RID(..), ROP(..), ST(..)) where

import AST(Literal(..))
}

%wrapper "basic"

$special   = [\(\)\,\;\[\]\`\{\}]

$return = \r
$linefeed = \n
$vertab = \v
$formfeed = \f
$space = \ 
$tab = \t

@nl = ($return $linefeed)
$rlf = [$return $linefeed $formfeed]
@newline = @nl | $rlf

$vst = [$vertab $space $tab]
@whitechar = @newline | $vst

-- Numbers and Characters

$digit = [0-9]
$ascLarge = [A-Z]
$ascSmall = [a-z]
$small = [$ascSmall \_]
$large = [$ascLarge]

$ascSymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$symbol = [$ascSymbol] # [$special \_ \: \" \']


$graphic = [$small $large $symbol $digit $special \: \" \']

@ANY = $graphic | @whitechar

$any = [$graphic $space $tab]

$anyns = $any # $symbol

-- Comments

@dashes = (\-\-) \-*

-- Whitespace

@comment = @dashes ($anyns  $any*)? @newline


@whitestuff = @whitechar | @comment
@whitespace = @whitestuff+

-- Integers

@decimal = $digit+

@integer = @decimal

-- Character

$asciic = [\000 \001 \002 \003 \004 \005 \006 \007 \008 \009 \010 \011 \012 \013 \014 \015
           \016 \017 \018 \019 \020 \021 \022 \023 \024 \025 \026 \027 \028 \029 \030 \031
           \032 \127]

$cntrl = [$ascLarge \@ \[ \] \^ \_]

$ascii = [[^$cntrl] $asciic]

$charesc = [abfnrtv\\\"\'\&]

$graphicwc = $graphic # [\'\\]

@escape = \92 ($charesc | $ascii)

@char = \' ($graphicwc | $space | @escape ) \'

-- String

@gap = \\ @whitechar+ \\

$graphicws = $graphic # [\"\\]

@string = \" ($graphicws | $space | @escape | gap)* \"


@sldq = ($small | $large | $digit | \')

@reservedop = (\.\. | \: | \:\: | \= | \\ | \| | \<\- | \-\> | \@ | \~ | \=\>)

@symsft = ($symbol | \:)*

@varsym = ($symbol @symsft)

@consym = (\: @symsft)

tokens :-

  @whitestuff   ;
  @char         { mkChar }
  @string       { mkString } -- TODO: remove space between gap using function on s.
  @integer      { mkInteger }
  $special      { mkSpecial }

  $small @sldq* { createVaridOrReserveID  }

  $large @sldq* { Conid }


  @varsym       { createXsymOrReservedOp Varsym  }
  @consym       { createXsymOrReservedOp Consym  }


{

data LToken = Comment String    
            | Literal Literal
            | Varid   String
            | Conid   String
            | ReserveID RID
            | Varsym String
            | Consym String
            | ReservedOP ROP
            | Special ST
  deriving (Eq,Show)


data ST = LParen
        | RParen
        | Comma
        | SemiColon
        | LSBracket
        | RSBracket
        | BTick
        | LCurly
        | RCurly
        deriving (Eq,Show)



data ROP = DDot
         | Colon
         | DColon
         | Equal
         | BSlash
         | Pipe
         | LArrow
         | RArrow
         | At
         | Sym
         | DArrow
  deriving (Eq,Show)

data RID = Case
         | Class
         | Data
         | Default
         | Deriving
         | Do
         | Else
         | If
         | Import
         | In
         | Infix
         | Infixl
         | Infixr
         | Instance
         | Let
         | Module
         | Newtype
         | Of
         | Then
         | Type
         | Where
         | Underscore
         deriving (Eq,Show)

createVaridOrReserveID :: String -> LToken
createVaridOrReserveID s = case s of
            "case" -> ReserveID Case
            "class" -> ReserveID Class
            "data" -> ReserveID Data
            "default" -> ReserveID Default
            "deriving" -> ReserveID Deriving
            "do" -> ReserveID Do
            "else" -> ReserveID Else
            "if" -> ReserveID If
            "import" -> ReserveID Import
            "in" -> ReserveID In
            "infix" -> ReserveID Infix
            "infixl" -> ReserveID Infixl
            "infixr" -> ReserveID Infixr
            "instance" -> ReserveID Instance
            "let" -> ReserveID Let
            "module" -> ReserveID Module
            "newtype" -> ReserveID Newtype
            "of" -> ReserveID Of
            "then" -> ReserveID  Then
            "type" -> ReserveID Type
            "where" -> ReserveID Where
            "_" -> ReserveID Underscore
            _      -> Varid     s


mkSpecial :: String -> LToken
mkSpecial s = Special specialToken
  where
    specialToken = case s of
         "("  -> LParen
         ")"  -> RParen
         ","  -> Comma
         ";"  -> SemiColon
         "["  -> LSBracket
         "]"  -> RSBracket
         "`"  -> BTick
         "{"  -> LCurly
         "}"  -> RCurly
         _    -> error "Lexing Error: Not a special token"


createXsymOrReservedOp :: (String -> LToken) -> String -> LToken
createXsymOrReservedOp symBuilder s =
    case tryBuildingReservedOP s of
      Just x -> ReservedOP x
      Nothing -> symBuilder s

tryBuildingReservedOP :: String -> Maybe ROP
tryBuildingReservedOP s = case s of
            ".." -> Just DDot
            ":"  -> Just Colon
            "::"  -> Just DColon
            "="  -> Just Equal
            "\\"  -> Just BSlash
            "|"  -> Just Pipe
            "<-"  -> Just LArrow
            "->"  -> Just RArrow
            "@"  -> Just At
            "~"  -> Just Sym
            "=>"  -> Just DArrow
            _   -> Nothing

mkChar :: String -> LToken
mkChar [_,c,_] = Literal $ TChar  c
mkChar _       = error "Invalid String"

mkString :: String -> LToken
mkString (_:r) = Literal $ TString $ take (length r - 1) r

mkInteger :: String -> LToken
mkInteger    = Literal . TInteger . read
}
