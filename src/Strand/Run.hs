
module Strand.Run where

import Language.ANTLR4
import Pseudoknot.Run (parseAndValidate)
import Pseudoknot.Semantic (DotBracketNotation(..))
import Strand.Grammar
import Strand.Parser
import Text.Pretty.Simple (pPrint)


strandRun :: IO ()
strandRun = do
  !input <- getContents
  let result = dotparParse input
  case result of
    (ResultAccept ast) -> do
      let
        (seqs, dpseq)  = ast2dotparformat ast
        (_, dpIndexed) = getIndexedDotPar 1 dpseq
        sequence       = printSequence seqs
        declarations   = printDeclarations dpIndexed
        structure      = printStructure $ repeatFilterDotPar dpIndexed
        result         = concat $
          [ "primary\n"
          , "# the primary structure is indexed starting from 1\n"
          , sequence
          , "\n"
          , "declare\n"
          , "# the basic loops, identified by the starting and ending indexes\n"
          , declarations
          , "\n"
          , "structure\n"
          , "# this is an expression using the nesting and concatenation operators\n"
          , structure
          , "\n"
          , "end\n"
          ]
      putStr result
    err -> print err


indexedDotParToList :: LabeledDotPar (Int, Int) -> [(Int, DotBracketNotation)]
indexedDotParToList dp = case dp of
  LDPEmpty          -> []
  LDPDot (i, _)     -> [(i, Dot)]
  LDPPar (i, j) sdp -> [(i, LeftBracket)] ++ indexedDotParToList sdp ++ [(j, RightBracket)]
  LDPConc dp1 dp2   -> indexedDotParToList dp1 ++ indexedDotParToList dp2


dotParToList :: DotPar -> [DotBracketNotation]
dotParToList dp = case dp of
  DPEmpty        -> []
  DPDot          -> [Dot]
  DPPar sdp      -> [LeftBracket] ++ dotParToList sdp ++ [RightBracket]
  DPConc dp1 dp2 -> dotParToList dp1 ++ dotParToList dp2

