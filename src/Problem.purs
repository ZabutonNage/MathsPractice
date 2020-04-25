module Problem (
  Problem(..),
  Result,
  generate
) where


import Prelude
import Effect (Effect)
import Effect.Random (randomInt)

import Data.Maybe (Maybe(..))


data Problem = Problem {
  a :: Int,
  op :: String,
  b :: Int,
  result :: Result
}

type Result = Int

generate :: Maybe Problem -> Effect Problem
generate Nothing = simpleAddition
generate justPrev@(Just prev) = do
  next <- simpleAddition
  if next == prev
    then generate justPrev
    else pure next


simpleAddition :: Effect Problem
simpleAddition = do
  a <- randomInt 1 10
  b <- randomInt 1 10
  pure $ Problem {
    a, b,
    op: "+",
    result: a + b
  }


instance showProblem :: Show Problem where
  show (Problem { a, b, op }) = show a <> " " <> op <> " " <> show b <> " = x"

instance eqProblem :: Eq Problem where
  eq (Problem p1) (Problem p2) = p1 == p2
