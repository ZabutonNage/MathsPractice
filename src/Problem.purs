module Problem (
  Problem(..),
  Result,
  generate
) where


import Prelude
import Effect (Effect)
import Effect.Random (randomInt)

import Data.Array (length, index)
import Data.Maybe (Maybe(..), fromMaybe)


data Problem = Problem {
  a :: Int,
  op :: String,
  b :: Int,
  result :: Result
}

type Result = Int

theProblem :: Effect Problem
theProblem = tenByTen


generate :: Maybe Problem -> Effect Problem
generate Nothing = theProblem
generate justPrev@(Just prev) = do
  next <- theProblem
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

coreMultiplication :: Effect Problem
coreMultiplication = multiplication [2, 2, 5, 5, 10]

tenByTen :: Effect Problem
tenByTen = multiplication [2, 3, 4, 5, 6, 7, 8, 9, 10]

multiplication :: Array Int -> Effect Problem
multiplication factors = do
  a_ <- randomInt 0 (length factors - 1) <#> (fromMaybe 1 <<< index factors)
  b_ <- randomInt 2 10
  order <- randomInt 0 1
  let { a, b } = if order == 1 then { a: a_, b: b_ } else { a: b_, b: a_ }

  pure $ Problem {
    a, b,
    op: "×",
    result: a * b
  }

instance showProblem :: Show Problem where
  show (Problem { a, b, op }) = show a <> " " <> op <> " " <> show b <> " = x"

derive instance eqProblem :: Eq Problem
