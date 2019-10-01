{
module Kreb.Lang.Lexer where

import qualified Data.Text.Lazy as T
import qualified Data.List as L

import qualified Kreb.Lang.LexicalGrammar as Lex
import Kreb.Lang.Loc
}

%wrapper "monadUserState"



-- Macros --

@whitespace = [ \  \t \n \r ]+

$hex = [ 0-9 a-f A-F ]
@int = [ 1-9 ] [ 0-9 ]*

@char = ' \\ 0 x $hex [$hex]* '

@esc_simp = [ n t v ' \" \\ ]

@name = [ a-z A-Z \_ ] [ a-z A-Z 0-9 \_ \- ' : ]*
@func = [ A-Z ] [ a-z A-Z 0-9 ' ]*
@tvar = [ a-z ] [ a-z A-Z 0-9 ]*
@svar = \$ [ A-Z ] [ a-z A-Z 0-9 ]*





tokens :-

-- ======= --
-- Phrases --
-- ======= --

-- Atoms
<0,def>  @name        { _w TokenWord                          KeepCode }
<0,def>  \#@name      { _w TokenBuiltIn                       KeepCode }

-- Constants
<0,def>  @int         { _w TokenInt                           KeepCode }
<0,def>  0            { _w TokenInt                           KeepCode }
<0,def>  '            { _t (TokenSymbol Lex.SY_SingleQuote)   (EnterCode chr) }
<0,def>  "            { _t (TokenSymbol Lex.SY_DoubleQuote)   (EnterCode chr) }

-- Quotations
<0,def>  [\[]         { _t (TokenSymbol Lex.SY_OpenBrack)     KeepCode }
<0,def>  [\]]         { _t (TokenSymbol Lex.SY_ClosedBrack)   KeepCode }



-- ============= --
-- Char Literals --
-- ============= --

<chr>  '              { _t (TokenSymbol Lex.SY_SingleQuote)   ExitCode }
<chr>  \"             { _t (TokenSymbol Lex.SY_DoubleQuote)   ExitCode }

<chr>  [\\]@esc_simp  { _w TokenChar                          KeepCode }

<chr>  [.]            { _w TokenChar                          KeepCode }



-- ================ --
-- Word Definitions --
-- ================ --

<0>    ^ \@define [\ \n] { _t (TokenKeyword Lex.KW_Define)       (EnterCode tok) }

<tok>  @name             { _w TokenWord                          (EnterCode sig) }

<sig>  [:][:]            { _t (TokenSymbol Lex.SY_DoubleColon)   KeepCode }
<sig>  [\$]              { _t (TokenSymbol Lex.SY_Dollar)        KeepCode }
<sig>  forall            { _t (TokenSymbol Lex.SY_ForAll)        KeepCode }
<sig>  Int               { _t (TokenTypeConst "Int")             KeepCode }
<sig>  Char              { _t (TokenTypeConst "Char")            KeepCode }
<sig>  String            { _t (TokenTypeConst "String")          KeepCode }
<sig>  \@Eff             { _w TokenEff                           KeepCode }
<sig>  @tvar             { _w TokenVarT                          KeepCode }
<sig>  @svar             { _w TokenVarS                          KeepCode }
<sig>  @func             { _w TokenConst                         KeepCode }
<sig>  \.                { _t (TokenSymbol Lex.SY_Dot)           KeepCode }
<sig>  [\-][>]           { _t (TokenSymbol Lex.SY_MinusGreater)  KeepCode }
<sig>  [\(]              { _t (TokenSymbol Lex.SY_OpenParen)     KeepCode }
<sig>  [\)]              { _t (TokenSymbol Lex.SY_ClosedParen)   KeepCode }

<sig>  [=][=]            { _t (TokenSymbol Lex.SY_DoubleEqual)   (EnterCode def) }

<def>  ^ \@end           { _t (TokenKeyword Lex.KW_End)          ResetCode }



-- ================= --
-- Data Declarations --
-- ================= --

<0>    ^ \@data [\ \n] { _t (TokenKeyword Lex.KW_Data)         (EnterCode dec) }

<dec>  [=]             { _t (TokenSymbol Lex.SY_Equal)         KeepCode }
<dec>  [\|]            { _t (TokenSymbol Lex.SY_Pipe)          KeepCode }
<dec>  [\$]            { _t (TokenSymbol Lex.SY_Dollar)        KeepCode }
<dec>  @tvar           { _w TokenVarT                          KeepCode }
<dec>  Int             { _t (TokenTypeConst "Int")             KeepCode }
<dec>  Char            { _t (TokenTypeConst "Char")            KeepCode }
<dec>  String          { _t (TokenTypeConst "String")          KeepCode }
<dec>  @func           { _w TokenConst                         KeepCode }
<dec>  [\-][>]         { _t (TokenSymbol Lex.SY_MinusGreater)  KeepCode }
<dec>  [\(]            { _t (TokenSymbol Lex.SY_OpenParen)     KeepCode }
<dec>  [\)]            { _t (TokenSymbol Lex.SY_ClosedParen)   KeepCode }

<dec> ^ \@end          { _t (TokenKeyword Lex.KW_End)          ResetCode }



@whitespace ;





{

data NextCode
  = EnterCode Int
  | ExitCode
  | KeepCode
  | ResetCode
  deriving (Eq, Show)


data AlexUserState = Trace
  { _tokens  :: [(Int, Token)]
  , _lastscd :: Maybe Int
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
  | TokenEff (String, Loc)

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
  -> NextCode
  -> (AlexPosn, Char, [Byte], String)
  -> Int
  -> Alex Token
_t tok next (pos, _, _, _) _ = do
  case next of
    KeepCode -> return ()
    EnterCode k -> do
      stashStartCode
      alexSetStartCode k
    ExitCode -> do
      k <- stealStartCode
      alexSetStartCode k
    ResetCode -> do
      stealStartCode
      alexSetStartCode 0
  let token = tok $ toLoc pos
  trace token
  return $ token

_w
  :: ((String, Loc) -> Token)
  -> NextCode
  -> (AlexPosn, Char, [Byte], String)
  -> Int
  -> Alex Token
_w tok next (pos, _, _, s) i = do
  case next of
    KeepCode -> return ()
    EnterCode k -> do
      stashStartCode
      alexSetStartCode k
    ExitCode -> do
      k <- stealStartCode
      alexSetStartCode k
    ResetCode -> do
      stealStartCode
      alexSetStartCode 0
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

stashStartCode :: Alex ()
stashStartCode = do
  k <- alexGetStartCode
  muts $ \st -> st
    { alex_ust = (alex_ust st) { _lastscd = Just k }
    }

stealStartCode :: Alex Int
stealStartCode = do
  k <- gets (_lastscd . alex_ust)
  muts $ \st -> st
    { alex_ust = (alex_ust st) { _lastscd = Nothing }
    }
  case k of
    Nothing -> return 0
    Just m -> return m

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
alexInitUserState = Trace [] Nothing

}
