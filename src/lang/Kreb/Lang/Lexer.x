{
module Kreb.Lang.Lexer where

import qualified Data.Text.Lazy as T
import qualified Data.List as L

import qualified Kreb.Lang.LexicalGrammar as Lex
import Kreb.Lang.Loc
}

%wrapper "monadUserState"

$whitechar = [\ \t\n\r]

$pos = [1-9]
$dec = [0-9]
$hex = [0-9a-fA-F]

@int = $pos [$dec]*

@char = ' \\ 0 x $hex [$hex]* '

$nameStart  = [\_ a-z A-Z]
$nameMiddle = [ 0-9 \- ' ]

@name = $nameStart [ $nameStart $nameMiddle ]*
@whitespace = [ $whitechar ]+

$funcStart  = [A-Z]
$funcMiddle = [a-zA-Z0-9']
@func = $funcStart [ $funcMiddle ]*

$tvarStart  = [a-z]
$tvarMiddle = [a-z0-9]
@tvar = $tvarStart [ $tvarMiddle ]*

$svarStart  = [A-Z]
$svarMiddle = [a-z0-9]
@svar = $svarStart [ $svarMiddle ]*

tokens :-

@whitespace ;

-- ================ --
-- Word Definitions --
-- ================ --

<0>  [\[]         { _t (TokenSymbol Lex.SY_OpenBrack)     Nothing }
<0>  [\]]         { _t (TokenSymbol Lex.SY_ClosedBrack)   Nothing }

<0>  @name        { _w TokenWord                          Nothing }
<0>  \#@name      { _w TokenBuiltIn                       Nothing }
<0>  @int         { _w TokenInt                           Nothing }
<0>  '.'          { _w TokenChar                          Nothing }
<0>  @char        { _w TokenChar                          Nothing }

<0> ^ \@define [\ \n] { _t (TokenKeyword Lex.KW_Define) (Just tok) }

<tok>  @name        { _w TokenWord (Just typ) }

<typ>  [:][:]       { _t (TokenSymbol Lex.SY_DoubleColon)   Nothing }
<typ>  [\$]         { _t (TokenSymbol Lex.SY_Dollar)        Nothing }
<typ>  forall       { _t (TokenSymbol Lex.SY_ForAll)        Nothing }
<typ>  Int          { _t (TokenTypeConst "Int")             Nothing }
<typ>  Char         { _t (TokenTypeConst "Char")            Nothing }
<typ>  String       { _t (TokenTypeConst "String")          Nothing }
<typ>  @tvar        { _w TokenVarT Nothing }
<typ>  @svar        { _w TokenVarS Nothing }
<typ>  @func        { _w TokenConst Nothing }
<typ>  \.           { _t (TokenSymbol Lex.SY_Dot)           Nothing }
<typ>  [\-][>]      { _t (TokenSymbol Lex.SY_MinusGreater)  Nothing }
<typ>  [\(]         { _t (TokenSymbol Lex.SY_OpenParen)     Nothing }
<typ>  [\)]         { _t (TokenSymbol Lex.SY_ClosedParen)   Nothing }

<typ>  [=][=]       { _t (TokenSymbol Lex.SY_DoubleEqual)   (Just def) }
<def>  [\[]         { _t (TokenSymbol Lex.SY_OpenBrack)     Nothing }
<def>  [\]]         { _t (TokenSymbol Lex.SY_ClosedBrack)   Nothing }

<def>  @name        { _w TokenWord                          Nothing }
<def>  \#@name      { _w TokenBuiltIn                       Nothing }
<def>  @int         { _w TokenInt                           Nothing }
<def>  '.'          { _w TokenChar                          Nothing }
<def>  @char        { _w TokenChar                          Nothing }

<def>  ^ \@end        { _t (TokenKeyword Lex.KW_End)          (Just 0) }

-- ================= --
-- Data Declarations --
-- ================= --

<0> ^ \@data [\ ]          { _t (TokenKeyword Lex.KW_Data)         (Just dec) }

<dec>  [=]          { _t (TokenSymbol Lex.SY_Equal)         Nothing }
<dec>  [\|]         { _t (TokenSymbol Lex.SY_Pipe)          Nothing }
<dec>  [\$]         { _t (TokenSymbol Lex.SY_Dollar)        Nothing }
<dec>  @tvar        { _w TokenVarT Nothing }
<dec>  Int          { _t (TokenTypeConst "Int")             Nothing }
<dec>  Char         { _t (TokenTypeConst "Char")            Nothing }
<dec>  String       { _t (TokenTypeConst "String")          Nothing }
<dec>  @func        { _w TokenConst Nothing }
<dec>  [\-][>]      { _t (TokenSymbol Lex.SY_MinusGreater)  Nothing }
<dec>  [\(]         { _t (TokenSymbol Lex.SY_OpenParen)     Nothing }
<dec>  [\)]         { _t (TokenSymbol Lex.SY_ClosedParen)   Nothing }

<dec> ^ \@end        { _t (TokenKeyword Lex.KW_End)          (Just 0) }




{
data AlexUserState = Trace
  { _tokens :: [(Int, Token)]
  }

instance Show AlexUserState where
  show x = L.unlines
    [ "-- Tokens --"
    , L.unlines $ map show $ reverse $ _tokens x
    ]

lexer :: (Token -> Parser a) -> Parser a
lexer = ((Parser $ fmap Right alexMonadScan) >>=)

newtype Parser a = Parser
  { unParser :: Alex (Either Token a)
  }

instance Functor Parser where
  fmap f = Parser . fmap (fmap f) . unParser

instance Applicative Parser where
  pure = Parser . pure . Right

  f <*> x = Parser $ do
    f' <- unParser f
    x' <- unParser x
    return $ f' <*> x'

instance Monad Parser where
  return = Parser . return . Right

  x >>= f = Parser $ do
    a <- unParser x
    case a of
      Left err -> return $ Left err
      Right ok -> unParser $ f ok


data Error
  = LexError String
  | ParseError Token
  deriving (Eq, Show)

runParser :: Parser a -> String -> Either Error a
runParser = runParser' 0

runParser' :: Int -> Parser a -> String -> Either Error a
runParser' mode p s =
  case runAlex s (alexSetStartCode mode >> unParser p) of
    Left msg -> Left $ LexError msg
    Right zs -> case zs of
      Left err -> Left $ ParseError err
      Right ok -> Right ok

data Token
  = TokenKeyword Lex.KeyWord Loc
  | TokenSymbol  Lex.Symbol  Loc
  | TokenWord    (String, Loc)
  | TokenBuiltIn (String, Loc)
  | TokenVarT (String, Loc)
  | TokenVarS (String, Loc)
  | TokenTypeConst String Loc
  | TokenConst (String, Loc)

  | TokenInt (String, Loc)
  | TokenChar (String, Loc)
  | EOF
  deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = return EOF

_skip
  :: (AlexPosn, Char, [Byte], String)
  -> Int
  -> Alex ()
_skip _ _ = return ()

_t
  :: (Loc -> Token)
  -> Maybe Int
  -> (AlexPosn, Char, [Byte], String)
  -> Int
  -> Alex Token
_t tok scd (pos, _, _, _) _ = do
  case scd of
    Nothing -> return ()
    Just k -> alexSetStartCode k
  let token = tok $ toLoc pos
  trace token
  return $ token

_w
  :: ((String, Loc) -> Token)
  -> Maybe Int
  -> (AlexPosn, Char, [Byte], String)
  -> Int
  -> Alex Token
_w tok scd (pos, _, _, s) i = do
  case scd of
    Nothing -> return ()
    Just k -> alexSetStartCode k
  let token = tok (take i s, toLoc pos)
  trace token
  return token 

toLoc :: AlexPosn -> Loc
toLoc (AlexPn chr ln col) = Loc ln col chr

_DEBUG_TRACE :: Bool
_DEBUG_TRACE = True

trace :: Token -> Alex ()
trace x = if _DEBUG_TRACE
  then do
    ust <- gets alex_ust
    scd <- gets alex_scd
    let tr = _tokens ust
    muts $ \ast -> ast
      { alex_ust = ust
        { _tokens = (scd, x) : tr
        }
      }
  else return ()

gets :: (AlexState -> a) -> Alex a
gets f = Alex $ \ast ->
  Right (ast, f ast)

muts :: (AlexState -> AlexState) -> Alex ()
muts f = Alex $ \ast ->
  Right (f ast, ())

debugAlex
  :: String -> Alex a -> Either String (AlexState, a)
debugAlex input__ (Alex f) = f $ AlexState
  { alex_bytes = []
  , alex_pos   = alexStartPos
  , alex_inp   = input__
  , alex_chr   = '\n'
  , alex_ust   = alexInitUserState
  , alex_scd   = 0
  }

asExpr :: Parser a -> Parser a
asExpr (Parser p) = Parser (alexSetStartCode def >> p)

debugParser :: (Show a) => Int -> Parser a -> String -> IO ()
debugParser mode p input = do
  putStrLn "\n===> Input:"
  putStrLn $ show input
  case debugAlex input (alexSetStartCode mode >> unParser p) of
    Left err -> do
      putStrLn "\n===> Lexical Error:"
      putStrLn err
    Right (st,zs) -> do
      case zs of
        Left tok -> do
          putStrLn "\n===> Parse Error at token:"
          putStrLn $ show tok
        Right ok -> do
          putStrLn "\n===> Parse Result:"
          putStrLn $ show ok
      putStrLn "\n===> Debug:"
      putStrLn $ show $ alex_ust st

alexInitUserState :: AlexUserState
alexInitUserState = Trace []

}
