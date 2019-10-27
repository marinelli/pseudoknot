
module Pseudoknot.Run where

import Control.Monad
import Language.ANTLR4
import Pseudoknot.Grammar
import Pseudoknot.Parser
import Pseudoknot.Semantic
import Text.ANTLR.Set as S (toList)
import Text.Pretty.Simple (pPrint)


getAST (ResultAccept ast) = ast
getAST _                  = error "non-AST in ResultSet"


run :: IO ()
run = do
  !input <- getContents

  -- putStrLn ">>> Input file:"
  -- putStrLn input
  -- putStrLn "<<<\n"
  -- putStrLn ">>> Parsing result:"

  let result = rnaParse input

  -- case result of
  --   (ResultAccept ast) -> pPrint $ ast2rna ast
  --   (ResultSet xs)     -> mapM_ (print . ast2rna . getAST) (S.toList xs)
  --   err                -> print err
  -- putStrLn "<<<\n"

  case result of
    (ResultAccept ast) -> do
      let (_, sequence, _, declarations, _, structure, _) = ast2rna ast

      let
        sequenceIsValid    = checkSequenceValidity . getSequence $ sequence
        mapSequence        = getSequenceMapping . getParsedSequence $ sequence
        unpairedNucleotide = checkDeclarationsUnpaired declarations
        validPairs         = checkDeclarationsValidPairs mapSequence declarations
        atMostOneBasicLoop = checkDeclarationsUniqPairs declarations
        exprValidConc      = checkLeftmostRightmostIndexes structure declarations
        exprValidNest      = checkNestingIndexes structure declarations

        constraints        =
          [ ( sequenceIsValid
            , "Sequence is not valid"
            )
          , ( unpairedNucleotide
            , "Not all basic loops contain an unpaired nucleotide"
            )
          , ( validPairs
            , "The primary sequence contains not valid pairs"
            )
          , ( atMostOneBasicLoop
            , "Not all nucleotides belong to at most one basic loop"
            )
          , ( exprValidConc
            , "The expression is not well concatenated"
            )
          , ( exprValidNest
            , "The expression is not well nested"
            )
          ]

      forM_ constraints (\ (c, e) -> unless c $ putStrLn $ "error: " <> e)
      unless (all fst constraints) $ errorWithoutStackTrace "error encountered"

      when (all fst constraints) $ do
        putStrLn $ insertEvery '\n' 50 $ getSequence sequence
        putStrLn $ insertEvery '\n' 50 $
          concatMap show $ getDotBracketNotation sequence declarations
    err -> print err


parseAndValidate input = do
  let result = rnaParse input
  case result of
    (ResultAccept ast) -> do
      let (_, sequence, _, declarations, _, structure, _) = ast2rna ast

      let
        sequenceIsValid    = checkSequenceValidity . getSequence $ sequence
        mapSequence        = getSequenceMapping . getParsedSequence $ sequence
        unpairedNucleotide = checkDeclarationsUnpaired declarations
        validPairs         = checkDeclarationsValidPairs mapSequence declarations
        atMostOneBasicLoop = checkDeclarationsUniqPairs declarations
        exprValidConc      = checkLeftmostRightmostIndexes structure declarations
        exprValidNest      = checkNestingIndexes structure declarations

        constraints        =
          [ ( sequenceIsValid
            , "Sequence is not valid"
            )
          , ( unpairedNucleotide
            , "Not all basic loops contain an unpaired nucleotide"
            )
          , ( validPairs
            , "The primary sequence contains not valid pairs"
            )
          , ( atMostOneBasicLoop
            , "Not all nucleotides belong to at most one basic loop"
            )
          , ( exprValidConc
            , "The expression is not well concatenated"
            )
          , ( exprValidNest
            , "The expression is not well nested"
            )
          ]

      return $ all fst constraints
    err -> return False

