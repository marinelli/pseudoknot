
module Pseudoknot.Grammar where

import Language.ANTLR4
import Language.ANTLR4 (Data(..))
import Text.ANTLR.Pretty
import Text.ANTLR.Set (Hashable(..), Generic(..))


data Keyword = Primary | Declare | Structure | End
  deriving (Eq, Show)

newtype Sequence = Sequence String
  deriving (Eq, Show)

getSequence :: Sequence -> String
getSequence (Sequence s) = s

newtype Declarations = Declarations [Declaration]
  deriving (Eq, Show)

getDeclarations :: Declarations -> [Declaration]
getDeclarations (Declarations ds) = ds

data Declaration = Declaration Var Number Number
  deriving (Eq, Show)

getDeclVar :: Declaration -> Var
getDeclVar (Declaration v _ _) = v

getDeclNumbers :: Declaration -> (Number, Number)
getDeclNumbers (Declaration _ n1 n2) = (n1, n2)

newtype Var = Var String
  deriving (Eq, Show, Ord)

getVar :: Var -> String
getVar (Var v) = v

newtype Number = Number Int
  deriving (Eq, Show, Ord)

getNumber :: Number -> Int
getNumber (Number n) = n


type StructExp = StructExp' String

data StructExp' t
  = ExpConc  (StructExp' t) (StructExp' t)
  | ExpNest  (StructExp' t) (StructExp' t)
  | ExpId    t
  | ExpEmpty
  deriving (Eq, Show)

instance Foldable StructExp'
  where
    foldMap f e = case e of
      ExpEmpty      -> mempty
      ExpId v       -> f v
      ExpNest e1 e2 -> foldMap f e1 `mappend` foldMap f e2
      ExpConc e1 e2 -> foldMap f e1 `mappend` foldMap f e2


emptyList :: [a]
emptyList = []

singletonList :: a -> [a]
singletonList = (: [])

consList :: a -> [a] -> [a]
consList = (:)


[g4|

  grammar Rna ;

  PrimaryKeyword      : 'primary'                       -> String ;
  DeclareKeyword      : 'declare'                       -> String ;
  StructureKeyword    : 'structure'                     -> String ;
  EndKeyword          : 'end'                           -> String ;
  fragment ALPHA      : [a-zA-Z] ;
  fragment DIGIT      : [0-9] ;
  NUMBER              : [0] | [1-9] DIGIT*              -> Int ;
  ID                  : ALPHA (ALPHA | DIGIT)*          -> String ;
  WS                  : [\r\n\t ]+                      -> String ;
  COMMENT             : '#' (~ [\r\n])*                 -> String ;

  rna :
    primary
    primarySequence
    declare
    declarationsOrEmpty
    structure
    structExp
    end
    ;

  primary             : PrimaryKeyword                  -> ${const Primary } ;
  primarySequence     : ID                              -> Sequence ;
  declare             : DeclareKeyword                  -> ${const Declare } ;
  var                 : ID                              -> Var ;
  num                 : NUMBER                          -> Number ;
  declaration         : var '=' '(' num ',' num ')'     -> Declaration ;
  declarations        : declaration                     -> singletonList
                      | declaration ',' declarations    -> consList ;
  declarationsOrEmpty : declarations                    -> id
                      |                                 -> emptyList ;
  structure           : StructureKeyword                -> ${const Structure } ;
  structExpOrEmpty    : structExp                       -> id
                      |                                 -> ExpEmpty ;
  structExp           : structExpConc                   -> id ;
  structExpConc       : structExpNest                   -> id
                      | structExpConc '+' structExpNest -> ExpConc ;
  structExpNest       : structExpSub                    -> id
                      | structExpNest '|' structExpId   -> ExpNest ;
  structExpSub        : structExpId                     -> id
                      | '(' structExp ')'               -> id ;
  structExpId         : var                             -> ${ExpId . getVar } ;
  end                 : EndKeyword                      -> ${const End} ;

|]


isWS :: RnaTSymbol -> Bool
isWS T_WS      = True
isWS T_COMMENT = True
isWS _         = False

