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
        
        ("\\+"     , mkFn ADD),
        ("\\-"     , mkFn SUB),
        ("\\*"     , mkFn MUL),
        ("\\/"     , mkFn QUO),
        ("\\("     , mkFn OPEN_PAREN),
        ("\\)"     , mkFn CLOSE_PAREN),
        ("\\,"     , mkFn COMMA),
        
        ("zero\\?" , mkFn ISZERO),
        ("equal\\?" , mkFn ISEQUAL),
        ("greater\\?" , mkFn ISGREATER),
        ("less\\?" , mkFn ISLESS),

        ("if"      , mkFn IF),
        ("then"    , mkFn THEN),
        ("else"    , mkFn ELSE),
        
        ("let"     , mkFn LET),
        ("in"      , mkFn IN),
        ("\\="     , mkFn EQ),
        
        ("[a-zA-Z][a-zA-Z0-9]*"    , mkFn IDENTIFIER)
      ]
  } 
