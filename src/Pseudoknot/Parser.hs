
module Pseudoknot.Parser
  ( rnaParse
  , ast2rna
  )
  where

import Pseudoknot.Grammar
import Language.ANTLR4
import Text.ANTLR.LR (GLRResult)


$(g4_parsers rnaAST rnaGrammar)

rnaParse :: [Char] -> GLRResult Int Char RnaToken RnaAST
rnaParse = glrParse isWS

