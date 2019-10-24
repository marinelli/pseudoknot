
module Pseudoknot.Semantic where

import Data.Char (toUpper)
import Data.Foldable (toList)
import Data.Maybe (isJust, mapMaybe)
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map
import Data.Set as Set (Set(..))
import qualified Data.Set as Set

import Pseudoknot.Grammar


data IupacNucleotideCode
  = A | C | G | T | U | W | S | M | K | R | Y | B | D | H | V | N | Z

  deriving (Eq, Show, Ord)


data DotBracketNotation
  = Dot | LeftBracket | RightBracket

  deriving (Eq)

instance Show DotBracketNotation
  where
    show s = case s of
      Dot          -> "."
      LeftBracket  -> "("
      RightBracket -> ")"


type ParsedSequence = [IupacNucleotideCode]

type MapSequence = Map Int IupacNucleotideCode

type MapDeclarations = Map Var (Number, Number)


charToIupac :: Char -> Maybe IupacNucleotideCode
charToIupac c =
  case toUpper c of
    'A' -> Just A
    'C' -> Just C
    'G' -> Just G
    'T' -> Just T
    'U' -> Just U
    'W' -> Just W
    'S' -> Just S
    'M' -> Just M
    'K' -> Just K
    'R' -> Just R
    'Y' -> Just Y
    'B' -> Just B
    'D' -> Just D
    'H' -> Just H
    'V' -> Just V
    'N' -> Just N
    'Z' -> Just Z
    _   -> Nothing


checkSequenceValidity :: String -> Bool
checkSequenceValidity inputSequence =
  foldr (\ x y -> isJust x && y) True maybeCodes
  where
    maybeCodes :: [Maybe IupacNucleotideCode]
    maybeCodes = map charToIupac inputSequence

getParsedSequence :: Sequence -> ParsedSequence
getParsedSequence s = mapMaybe charToIupac $ getSequence s

getSequenceMapping :: ParsedSequence -> MapSequence
getSequenceMapping p = Map.fromList list
  where
    list = zip [1..] p

checkDeclarationUnpaired :: Declaration -> Bool
checkDeclarationUnpaired d = num1 + 1 < num2
  where
    num1 = getNumber . fst . getDeclNumbers $ d
    num2 = getNumber . snd . getDeclNumbers $ d


checkDeclarationsUnpaired :: [Declaration] -> Bool
checkDeclarationsUnpaired = all checkDeclarationUnpaired


validWatsonCrickBasePairs :: Set (IupacNucleotideCode, IupacNucleotideCode)
validWatsonCrickBasePairs = Set.fromList
  [ (A, U)
  , (U, A)
  , (C, G)
  , (G, C)
  ]

validWobbleBasePairs :: Set (IupacNucleotideCode, IupacNucleotideCode)
validWobbleBasePairs = Set.fromList
  [ (G, U)
  , (U, G)
  ]

validBasePairs :: Set (IupacNucleotideCode, IupacNucleotideCode)
validBasePairs = Set.union validWatsonCrickBasePairs validWobbleBasePairs

checkDeclarationPairs :: MapSequence -> Declaration -> Bool
checkDeclarationPairs s d =
  case (nucleotide1, nucleotide2) of
    (Just x, Just y) -> Set.member (x, y) validBasePairs
    _                -> False
  where
    (Number n1, Number n2) = getDeclNumbers d
    nucleotide1            = Map.lookup n1 s
    nucleotide2            = Map.lookup n2 s


checkDeclarationsValidPairs :: MapSequence -> [Declaration] -> Bool
checkDeclarationsValidPairs s = all (checkDeclarationPairs s)


checkDeclarationsUniqPairs :: [Declaration] -> Bool
checkDeclarationsUniqPairs ds = length listIndexes == length setIndexes
  where
    listIndexes = foldr
      (\ x y -> fst (getDeclNumbers x) : snd (getDeclNumbers x) : y) [] ds
    setIndexes  = Set.fromList listIndexes


getDeclarationsMapping :: [Declaration] -> MapDeclarations
getDeclarationsMapping ds = Map.fromList list
  where
    list = map (\ d -> (getDeclVar d, getDeclNumbers d)) ds

varsFromDeclarations :: [Declaration] -> [Var]
varsFromDeclarations = map getDeclVar

checkUniqVarsDecl :: [Declaration] -> Bool
checkUniqVarsDecl ds = length listOfVars == length setOfVars
  where
    listOfVars = varsFromDeclarations ds
    setOfVars  = Set.fromList listOfVars

varsFromExpression :: StructExp -> [Var]
varsFromExpression = foldMap (\x -> [Var x])

checkUniqVarsExpr :: StructExp -> Bool
checkUniqVarsExpr exp = length listOfVars == length setOfVars
  where
    listOfVars = varsFromExpression exp
    setOfVars  = Set.fromList listOfVars

getIndexesFromExp :: StructExp -> [Declaration] -> [Number]
getIndexesFromExp exp decl = listOfIndexes
  where
    declMapping    = getDeclarationsMapping decl
    listOfVars     = varsFromExpression exp
    listOfIndexes  = foldr
      (\ x y -> maybe y (\ p -> fst p : snd p : y) $ Map.lookup x declMapping)
        [] listOfVars

leftmostPairedNucleotideIndex :: StructExp -> [Declaration] -> Maybe Number
leftmostPairedNucleotideIndex exp decl =
  case decl of
    [] -> Nothing
    _  -> Just $ minimum $ getIndexesFromExp exp decl

rightmostPairedNucleotideIndex :: StructExp -> [Declaration] -> Maybe Number
rightmostPairedNucleotideIndex exp decl =
  case decl of
    [] -> Nothing
    _  -> Just $ maximum $ getIndexesFromExp exp decl

checkLeftmostRightmostIndexes :: StructExp -> [Declaration] -> Bool
checkLeftmostRightmostIndexes exp decl =
  case exp of
    ExpConc e1 e2 ->
      let
        rightmostIndexE1 = rightmostPairedNucleotideIndex e1 decl
        leftmostIndexE2  = leftmostPairedNucleotideIndex e2 decl
      in
        rightmostIndexE1 < leftmostIndexE2
          && checkLeftmostRightmostIndexes e1 decl
          && checkLeftmostRightmostIndexes e2 decl
    ExpNest e1 e2 ->
      checkLeftmostRightmostIndexes e1 decl
        && checkLeftmostRightmostIndexes e2 decl
    ExpId _       -> True
    ExpEmpty      -> True


checkNestingSubExpressions :: StructExp -> Bool
checkNestingSubExpressions exp =
  case exp of
    ExpConc e1 e2 ->
      checkNestingSubExpressions e1
        && checkNestingSubExpressions e2
    ExpNest e1 (ExpId _) ->
      checkNestingSubExpressions e1
    ExpNest _ _          -> False
    ExpId _              -> True
    ExpEmpty             -> True

checkNestingIndexes :: StructExp -> [Declaration] -> Bool
checkNestingIndexes exp decl =
  case exp of
    ExpConc e1 e2 ->
      checkNestingIndexes e1 decl
        && checkNestingIndexes e2 decl
    ExpNest e (ExpId v) ->
      let
        var     = Var v
        varNums = Map.lookup var declMapping
        eLeft   = leftmostPairedNucleotideIndex e decl
        eRight  = rightmostPairedNucleotideIndex e decl
      in
        case (varNums, eLeft, eRight) of
          (Just p, Just el, Just er) ->
            fst p < el && el < er && er < snd p
              && checkNestingIndexes e decl
          _ -> False
    ExpNest _ _          -> False
    ExpId _              -> True
    ExpEmpty             -> True
  where
    declMapping = getDeclarationsMapping decl

getIndexesFromDeclarations :: [Declaration] -> Map Number DotBracketNotation
getIndexesFromDeclarations decl = Map.fromList indexes
  where
    indexes = foldr
      ( \ x y ->
        let (n1, n2) = getDeclNumbers x
        in
          (n1, LeftBracket) : (n2, RightBracket) : y
      ) [] decl

getDotBracketNotation :: Sequence -> [Declaration] -> [DotBracketNotation]
getDotBracketNotation sequence decl =
  Map.elems $ Map.unionWith const indexes dots
  where
    n       = length $ getParsedSequence sequence
    dots    = Map.fromList $ take n $ zip (map Number [1..]) (repeat Dot)
    indexes = getIndexesFromDeclarations decl

insertEvery :: a -> Int -> [a] -> [a]
insertEvery v n xs =
  let
    hd = take n xs
    tl = drop n xs
  in
    case (hd, tl) of
      (_, []) -> hd
      _       -> hd ++ [v] ++ insertEvery v n tl

