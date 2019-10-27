
module Strand.Parser
  ( dotparParse
  , ast2dotparformat
  )
  where

import Language.ANTLR4
import Pseudoknot.Grammar (Sequence(..))
import Strand.Grammar
import Text.ANTLR.LR (GLRResult)


$(g4_parsers dotParAST dotParGrammar)

dotparParse :: [Char] -> GLRResult Int Char DotParToken DotParAST
dotparParse = glrParse isWS

