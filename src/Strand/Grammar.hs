
module Strand.Grammar where

import Data.List (intersperse)
import Language.ANTLR4
import Language.ANTLR4 (Data(..))
import Pseudoknot.Grammar (Sequence(..), getSequence)
import Text.ANTLR.Pretty
import Text.ANTLR.Set (Hashable(..), Generic(..))


data DotPar
  = DPEmpty
  | DPDot
  | DPPar DotPar
  | DPConc DotPar DotPar
  deriving (Eq, Show)

data LabeledDotPar t
  = LDPEmpty
  | LDPDot t
  | LDPPar t (LabeledDotPar t)
  | LDPConc (LabeledDotPar t) (LabeledDotPar t)
  deriving (Eq, Show)


getIndexedDotPar :: Int -> DotPar -> ((Int, Int), LabeledDotPar (Int, Int))
getIndexedDotPar index dp = case dp of
  DPEmpty          -> ((index, index), LDPEmpty)
  DPDot            -> ((index + 1, index + 1), LDPDot (index, index))
  DPPar dp'        ->
    let ((_, j), ldp) = getIndexedDotPar (index + 1) dp'
    in
      ((j + 1, j + 1) , LDPPar (index, j) ldp)
  DPConc dp1 dp2   ->
    let ((i', j' ), ldp1) = getIndexedDotPar index dp1
        ((_ , j''), ldp2) = getIndexedDotPar j'    dp2
    in
      ((i', j''), LDPConc ldp1 ldp2)

printSequence :: [Sequence] -> String
printSequence = concat . map getSequence

printDeclarations :: LabeledDotPar (Int, Int) -> String
printDeclarations = concat . intersperse ", " . printDeclarations'

printDeclarations' :: LabeledDotPar (Int, Int) -> [String]
printDeclarations' ldp = case ldp of
  LDPEmpty           -> []
  LDPDot _           -> []
  LDPPar (x, y) ldp' -> [concat ["h", show x, " = (", show x, ",", show y, ")"]] ++ printDeclarations' ldp'
  LDPConc ldp1 ldp2  -> printDeclarations' ldp1 ++ printDeclarations' ldp2

filterDotPar :: LabeledDotPar (Int, Int) -> LabeledDotPar (Int, Int)
filterDotPar ldp = case ldp of
  LDPEmpty              -> LDPEmpty
  LDPDot _              -> LDPEmpty
  LDPPar p ldp'         -> LDPPar p (filterDotPar ldp')
  LDPConc ldp1 LDPEmpty -> filterDotPar ldp1
  LDPConc LDPEmpty ldp2 -> filterDotPar ldp2
  LDPConc ldp1 ldp2     -> LDPConc (filterDotPar ldp1) (filterDotPar ldp2)

repeatFilterDotPar :: LabeledDotPar (Int, Int) -> LabeledDotPar (Int, Int)
repeatFilterDotPar ldp =
  if ldp == filterDotPar ldp
    then ldp
    else repeatFilterDotPar $ filterDotPar ldp

printStructure :: LabeledDotPar (Int, Int) -> String
printStructure = concat . printStructure'

printStructure' :: LabeledDotPar (Int, Int) -> [String]
printStructure' ldp = case ldp of
  LDPEmpty               -> []
  LDPDot _               -> []
  LDPPar (x, y) LDPEmpty -> ["h" <> show x]
  LDPPar (x, y) ldp'     -> printStructure' ldp' ++ [" | " <> "h" <> show x]
  LDPConc ldp1 ldp2      -> ["("] ++ printStructure' ldp1 ++ [" + "] ++ printStructure' ldp2 ++ [")"]


[g4|

  grammar DotPar ;

  fragment NUCLEOTIDE : [AaCcGgTtUuWwSsMmKkRrYyBbDdHhVvNnZz]  -> Char ;
  fragment EXTRACODES : [P]                                   -> Char ;
  SEQUENCE            : (NUCLEOTIDE | EXTRACODES)+            -> String ;
  WS                  : [\r\n\t ]+                            -> String ;
  COMMENT             : '#' (~ [\r\n])*                       -> String ;

  dotparformat :
    sequence+
    dotpar
    ;

  sequence            : SEQUENCE                              -> Sequence ;
  dotpar              : dotpar1 dotpar                        -> DPConc
                      | dotpar1                               -> id
                      |                                       -> DPEmpty ;
  dotpar1             : '.'                                   -> DPDot
                      | '['                                   -> DPDot
                      | ']'                                   -> DPDot
                      | '{'                                   -> DPDot
                      | '}'                                   -> DPDot
                      | '<'                                   -> DPDot
                      | '>'                                   -> DPDot
                      | 'A'                                   -> DPDot
                      | 'a'                                   -> DPDot
                      | 'B'                                   -> DPDot
                      | 'b'                                   -> DPDot
                      | 'C'                                   -> DPDot
                      | 'c'                                   -> DPDot
                      | 'D'                                   -> DPDot
                      | 'd'                                   -> DPDot
                      | 'E'                                   -> DPDot
                      | 'e'                                   -> DPDot
                      | 'F'                                   -> DPDot
                      | 'f'                                   -> DPDot
                      | 'G'                                   -> DPDot
                      | 'g'                                   -> DPDot
                      | 'H'                                   -> DPDot
                      | 'h'                                   -> DPDot
                      | '(' dotpar ')'                        -> DPPar ;

|]


isWS :: DotParTSymbol -> Bool
isWS T_WS      = True
isWS T_COMMENT = True
isWS _         = False

