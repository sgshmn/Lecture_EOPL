module Lexer(lexerSpec) where

import Prelude hiding (EQ)
import CommonParserUtil
import Token

mkFn :: Token -> LexAction Token IO ()
mkFn tok = \text -> return $ Just tok

skip :: LexAction Token IO ()
skip = \text -> return $ Nothing

lexerSpec :: LexerSpec Token IO ()
lexerSpec = LexerSpec
  {
    endOfToken    = END_OF_TOKEN,
    lexerSpecList = 
      [ ("[ \t\n]" , skip),
        
        ("[0-9]+"  , mkFn INTEGER_NUMBER),
        
        ("->"      , mkFn ARROW),
        
        ("\\-"     , mkFn SUB),
        ("\\("     , mkFn OPEN_PAREN),
        ("\\)"     , mkFn CLOSE_PAREN),
        ("\\["     , mkFn OPEN_BRACKET),
        ("\\]"     , mkFn CLOSE_BRACKET),        
        ("\\,"     , mkFn COMMA),
        
        (":"       , mkFn COLON),
        
        ("zero\\?" , mkFn ISZERO),
        
        ("\\="     , mkFn EQ),
        
        ("[_a-zA-Z][_a-zA-Z0-9\\?]*"    , keywordOrIdentifier)
      ]
  } 

keywordOrIdentifier text =
  case lookup text keywords of
    Nothing  -> mkFn IDENTIFIER text
    Just tok -> mkFn tok text

keywords =
  [ ("if",     IF)
  , ("then",   THEN)
  , ("else",   ELSE)
  , ("letrec", LETREC)
  , ("let",    LET)
  , ("proc",   PROC)
  , ("in",     IN)
  , ("int",    TYINT)
  , ("bool",   TYBOOL) 
  , ("module", MODULE)
  , ("interface", INTERFACE)
  , ("body",   BODY)
  , ("from",   FROM)
  , ("take",   TAKE)
  , ("opaque", OPAQUE)
  , ("transparent", TRANSPARENT)
  , ("type",   TYPE)
  ]