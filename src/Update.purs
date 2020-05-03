module Update (
  update
) where


import Prelude
import Effect.Unsafe (unsafePerformEffect)

import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)

import Model
import Message
import Problem


update :: Model -> Message -> Model
update model Ignore               = model
update model (ChangeGameMode inp) = model  -- TODO focus limit input
                                    # parseGameMode inp
                                    # resetAnswer
update model (ReadLimit inp)      = processLimitInput inp model
update model StartLimit           = startLimit model
update model StopLimit            = stopLimit model
update model (ReadAnswer inp)     = setAnswer inp model
update model (TrySolve inp)       = case fromString inp of
                                      Nothing -> resetAnswer model
                                      Just answ -> next answ model


next :: Int -> Model -> Model
next answ model
  | isLimit model.mode = nextLimit answ model
  | true               = nextEndless answ model

nextEndless :: Int -> Model -> Model
nextEndless answ model
  | isCorrect answ model.problem = model
                                   # setNewProblem
                                   # resetAnswer
  | true = resetAnswer model

nextLimit :: Int -> Model -> Model
nextLimit answ model@{ lastCorrect, correctCount, probsToGo }
  | isCorrect answ model.problem = model
                                   # setCorrectCount (if lastCorrect then correctCount + 1 else correctCount)
                                   # setLastCorrect true
                                   # setProbsToGo (probsToGo - 1)
                                   # if probsToGo == 1
                                     then setLimitRunning false
                                          >>> setLimitCompleted true
                                     else resetAnswer
                                          >>> setNewProblem
  | true = model
           # setLastCorrect false
           # resetAnswer


parseGameMode :: String -> Model -> Model
parseGameMode "Endless" model = setGameMode Endless      model
parseGameMode "Limit"   model = setGameMode (Limit "20") model
parseGameMode _         model = model

processLimitInput :: String -> Model -> Model
processLimitInput lim model@{ mode: Limit _ } = model
                                                # setGameMode (Limit lim)
                                                # setLimitCompleted false
processLimitInput _   model                   = model

startLimit :: Model -> Model
startLimit model@{ mode: Limit lim } = model  -- TODO focus answer box
                                       # setLimitCompleted false
                                       # setLastCorrect true
                                       # setProbsToGo (fromMaybe 0 (fromString lim))
                                       # setCorrectCount 0
                                       # resetAnswer
                                       # setLimitRunning true
                                       # setNewProblem
startLimit model = model

stopLimit :: Model -> Model
stopLimit model = model
                  # setLimitRunning false
                  # setLimitCompleted false
                  # resetAnswer

resetAnswer :: Model -> Model
resetAnswer = setAnswer ""

isCorrect :: Result -> Problem -> Boolean
isCorrect res (Problem { result }) = res == result

setNewProblem :: Model -> Model
setNewProblem model = model # setProblem (unsafePerformEffect $ generate (Just model.problem))
