
module Strand.Parser
  ( dotparParse
  , ast2dotparformat
  )
  where

import Strand.Grammar
import Language.ANTLR4
import Text.ANTLR.LR (GLRResult)
import Pseudoknot.Grammar (Sequence(..))


$(g4_parsers dotParAST dotParGrammar)

dotparParse :: [Char] -> GLRResult Int Char DotParToken DotParAST
dotparParse = glrParse isWS

