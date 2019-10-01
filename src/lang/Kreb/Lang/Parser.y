{
-- Work around https://github.com/simonmar/happy/issues/109
-- #undef __GLASGOW_HASKELL__
-- #define __GLASGOW_HASKELL__ 709

module Kreb.Lang.Parser where

import Prelude hiding (Word)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Kreb.Lang.Lexer
import qualified Kreb.Lang.LexicalGrammar as Lex
import Kreb.Lang.Module
import Kreb.Lang.Type
import Kreb.Lang.Expr

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
  'quote' { TokenBuiltIn ("#quote", $$) }
  'compose' { TokenBuiltIn ("#compose", $$) }

  'repeat' { TokenBuiltIn ("#repeat", $$) }

  'int_plus' { TokenBuiltIn ("#int_plus", $$) }
  'int_times' { TokenBuiltIn ("#int_times", $$) }

  'string_concat' { TokenBuiltIn ("#string_concat", $$) }

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
  '\''  { TokenSymbol Lex.SY_SingleQuote $$ }
  '"'  { TokenSymbol Lex.SY_DoubleQuote $$ }
  'forall'  { TokenSymbol Lex.SY_ForAll $$ }

  'Int' {TokenTypeConst "Int" $$}
  'Char' {TokenTypeConst "Char" $$}
  'String' {TokenTypeConst "String" $$}
  '@Eff' { TokenEff $$ }

  tvar { TokenVarT $$ }
  svar { TokenVarS $$ }

%%

listOf(p)
  : p                                                  { [$1] }
  | listOf(p) p                                        { ($2:$1) }



module ::           { Module }
  : listOf( block ) { Module $1 }

block ::         { Decl }
  : define_block { $1 }
  | data_block   { $1 }



define_block ::                                    { Decl }
  : '@define' atom '::' scheme '==' phrase '@end'  { Definition (Atom (fst $2)) $6 $4 }



data_block ::                                       { Decl }
  : '@data' func data_clauses '@end'                { DeclareData (fst $2, []) $3 }
  | '@data' func listOf( tvar ) data_clauses '@end' { DeclareData (fst $2, map (V . fst) $3) $4 }

data_clauses ::                                               { [(Atom, [Type])] }
  : '=' func listOf( data_clauses_extra )                     { (Atom $ fst $2, []) : $3 }
  | '=' func listOf( type_atom ) listOf( data_clauses_extra ) { (Atom $ fst $2, $3) : $4 }

data_clauses_extra ::            { (Atom, [Type]) }
  : '|' func                     { (Atom $ fst $2, []) }
  | '|' func listOf( type_atom ) { (Atom $ fst $2, $3) }



scheme :: { Scheme }
  : arrow { quantify $1 }

arrow :: { Arrow }
  : stack '->' stack { Arrow $1 $3 }

stack :: { Stack }
  : svar                     { Stack (V (tail $ fst $1)) [] }
  | svar listOf( type_atom ) { Stack (V (tail $ fst $1)) (reverse $2) }




type_atom ::          { Type }
  : tvar              { TyVar $ V $ fst $1 }
  | type_prim         { TyCon $1 }
  | func              { TyCon $ C $ fst $1 }
  | '(' type_expr ')' { $2 }

type_prim :: { C Type }
  : 'Int'    { C "Int" }
  | 'Char'   { C "Char" }
  | 'String' { C "String" }
  | '@Eff'   { C "@Eff" }

type_func :: { Type }
  : func                { TyCon $ C $ fst $1 }
  | type_func type_atom { TyApp $1 $2 }

type_expr :: { Type }
  : type_func { $1 }
  | arrow     { TyArr $1 }





phrase ::       { Phrase }
  : word        { Then $1 Silence }
  | word phrase { Then $1 $2 }

word ::            { Word }
  : atom           { Only (Atom (fst $1)) }
  | builtin        { BuiltIn $1 }
  | '[' ']'        { Noop }
  | '[' phrase ']' { Quote $2 }

builtin ::         { BuiltIn }
  : int            { BuiltIn_Int (read (fst $1)) }
  | '\'' char '\''   { BuiltIn_Char (readStrChr (fst $2)) }

  | '"' listOf( char ) '"' { BuiltIn_String (reverse $ map (readStrChr . fst) $2) }

  | 'int_plus'     { BuiltIn_Int_Plus }
  | 'int_times'    { BuiltIn_Int_Times }

  | 'string_concat'    { BuiltIn_String_Concat }

  | 'id'           { BuiltIn_Id }
  | 'swap'         { BuiltIn_Swap }
  | 'apply'        { BuiltIn_Apply }
  | 'quote'        { BuiltIn_Quote }
  | 'compose'      { BuiltIn_Compose }

  | 'repeat'      { BuiltIn_Repeat }

  | custom_builtin { BuiltIn_Ext (fst $1) }



{
parseError :: Token -> Parser a
parseError = Parser . return . Left
}
