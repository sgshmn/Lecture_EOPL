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
        
        ("\\-"     , mkFn SUB),
        ("\\("     , mkFn OPEN_PAREN),
        ("\\)"     , mkFn CLOSE_PAREN),
        ("\\,"     , mkFn COMMA),
        
        ("zero\\?" , mkFn ISZERO),

        ("if"      , mkFn IF),
        ("then"    , mkFn THEN),
        ("else"    , mkFn ELSE),
        
        ("letrec"  , mkFn LETREC),

        ("let"     , mkFn LET),
        ("in"      , mkFn IN),
        ("\\="     , mkFn EQ),
        
        ("proc"    , mkFn PROC),

        ("begin"    , mkFn BEGIN),
        ("end"    , mkFn END),
        (";"    , mkFn SEMICOLON),

        ("set"    , mkFn SET),

        -- New tokens in classes.
        ("class"   , mkFn CLASS),
        ("extends" , mkFn EXTENDS),
        ("method"  , mkFn METHOD),
        ("field"   , mkFn FIELD),
        ("new"     , mkFn NEW),
        ("send"    , mkFn SEND),
        ("self"    , mkFn SELF),
        ("super"   , mkFn SUPER),
        -- New tokens in classes should be before IDENTIFIER
        -- to avoid matching them as identifiers.

        ("[a-zA-Z][a-zA-Z0-9]*"    , mkFn IDENTIFIER)
      ]
  } 
