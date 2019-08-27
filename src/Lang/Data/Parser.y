{
-- Work around https://github.com/simonmar/happy/issues/109
-- #undef __GLASGOW_HASKELL__
-- #define __GLASGOW_HASKELL__ 709

module Lang.Data.Parser where

import Prelude hiding (Word)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Lang.Data.Lexer
import qualified Lang.Data.LexicalGrammar as Lex
import Lang.Data.Module
import Lang.Data.Type
import Lang.Data.Expr

}

%name pModule module
%name pPhrase phrase

%tokentype { Token }
%error { parseError }
%lexer { lexer } { EOF }
%monad { Parser }



%token

  atom { TokenWord $$ }

  'id' { TokenBuiltIn ("#id", $$) }
  'swap' { TokenBuiltIn ("#swap", $$) }
  'apply' { TokenBuiltIn ("#apply", $$) }

  'int_plus' { TokenBuiltIn ("#int_plus", $$) }
  'int_times' { TokenBuiltIn ("#int_times", $$) }

  custom_builtin { TokenBuiltIn $$ }

  int { TokenInt $$ }
  char { TokenChar $$ }

  func { TokenConst $$ }

  '@define' { TokenKeyword Lex.KW_Define $$ }
  '@data'   { TokenKeyword Lex.KW_Data $$ }
  '@end'    { TokenKeyword Lex.KW_End $$ }

  '::' { TokenSymbol Lex.SY_DoubleColon $$ }
  '==' { TokenSymbol Lex.SY_DoubleEqual $$ }
  '->' { TokenSymbol Lex.SY_MinusGreater $$ }
  '$'  { TokenSymbol Lex.SY_Dollar $$ }
  '['  { TokenSymbol Lex.SY_OpenBrack $$ }
  ']'  { TokenSymbol Lex.SY_ClosedBrack $$ }
  '('  { TokenSymbol Lex.SY_OpenParen $$ }
  ')'  { TokenSymbol Lex.SY_ClosedParen $$ }
  '='  { TokenSymbol Lex.SY_Equal $$ }
  '|'  { TokenSymbol Lex.SY_Pipe $$ }
  '.'  { TokenSymbol Lex.SY_Dot $$ }
  'forall'  { TokenSymbol Lex.SY_ForAll $$ }

  'Int' {TokenTypeConst "Int" $$}
  'Char' {TokenTypeConst "Char" $$}
  'String' {TokenTypeConst "String" $$}

  tvar { TokenVarT $$ }
  svar { TokenVarS $$ }

%%

listOf(p)
  : p                                                  { [$1] }
  | listOf(p) p                                        { ($2:$1) }

module :: { Module }
  : blocks { Module $1 }

blocks :: { [Decl] }
  : listOf(block) { $1 }

block :: { Decl }
  : define_block { $1 }

define_block :: { Decl }
  : '@define' atom '::' scheme '==' phrase '@end'  { Definition (Atom (fst $2)) $6 $4 }

scheme :: { Scheme }
  : arrow { quantify $1 }

arrow :: { Arrow }
  : stack '->' stack { Arrow $1 $3 }

stack :: { Stack }
  : svar    { Stack (V (fst $1)) [] }
  | svar listOf(type) { Stack (V (fst $1)) (reverse $2) }

type :: { Type }
  : tvar   { TyVar (V (fst $1)) }
  | prim   { TyCon $1 }
  | '(' arrow ')' { TyArr $2 }

prim :: { C Type }
  : 'Int'    { C "Int" }
  | 'Char'   { C "Char" }
  | 'String' { C "String" }

phrase :: { Phrase }
  : word  { Then $1 Silence }
  | word phrase { Then $1 $2 }

word :: { Word }
  : atom   { Only (Atom (fst $1)) }
  | builtin { BuiltIn $1 }
  | '[' phrase ']' { Quote $2 }

builtin :: { BuiltIn }
  : int { BuiltIn_Int (read (fst $1)) }
  | char { BuiltIn_Char (read (fst $1)) }

  | 'int_plus' { BuiltIn_Int_Plus }
  | 'int_times' { BuiltIn_Int_Times }

  | 'id' { BuiltIn_Id }
  | 'swap' { BuiltIn_Swap }
  | 'apply' { BuiltIn_Apply }

  | custom_builtin { BuiltIn_Ext (fst $1) }


{
parseError :: Token -> Parser a
parseError = Parser . return . Left
}
























