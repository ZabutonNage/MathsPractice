module Update (
  update
) where


import Prelude
import Effect.Unsafe (unsafePerformEffect)

import Data.Int (fromString)
import Data.Maybe (Maybe(..))

import Model
import Message
import Problem


update :: Model -> Message -> Model
update model Ignore          = model
update model (ReadInput inp) = setAnswer inp model
update model (TrySolve inp)  = case fromString inp of
                                 Nothing -> resetAnswer model
                                 Just x
                                   | isCorrect x model.problem -> {
                                     answer: "",
                                     problem: unsafePerformEffect $ generate (Just model.problem)
                                   }
                                   | true -> resetAnswer model


isCorrect :: Result -> Problem -> Boolean
isCorrect res (Problem { result }) = res == result

resetAnswer :: Model -> Model
resetAnswer = setAnswer ""
