
module Pseudoknot.Parser
  ( rnaParse
  , ast2rna
  )
  where

import Language.ANTLR4
import Pseudoknot.Grammar
import Text.ANTLR.LR (GLRResult)


$(g4_parsers rnaAST rnaGrammar)

rnaParse :: [Char] -> GLRResult Int Char RnaToken RnaAST
rnaParse = glrParse isWS

