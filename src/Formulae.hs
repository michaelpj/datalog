module Formulae where

import Data.Vector as V

data Variable = Variable String
data PredRef = PredRef String

data FormulaF a =
  And a a
  | Or a a
  | Exists Variable a
  | Call PredRef (V.Vector Variable)

type Attributes = V.Vector Variable

data Value =
  Value Int
  | Unconstrained
  deriving (Eq)

data Assignment = Assignment Attributes (V.Vector Value)

data Assignments = Assignments (V.Vector Assignment)

naturalJoin :: Assignments -> Assignments -> Assignments

project :: Variable -> Assignments -> Assignments
project f (Rel ts) = Rel $ fmap (projectTuple f) ts

removeElem :: V.Vector a -> Int -> V.Vector a
removeElem v i =
  let (fst, last) = V.splitAt i v
  in (init fst) <> last

projectTuple :: Variable -> Assignment -> Assignment
projectTuple f t@(Tuple attrs vals) =
  case V.elemIndex f attrs of
    Nothing -> t
    Just i -> Tuple (removeElem attrs i) (removeElem vals i)

evalAlgebra :: FormulaF Assignments -> Assignments
