module Model (
  Model,
  GameMode(..),
  isLimit,
  setAnswer,
  setProblem,
  setGameMode,
  setLimitRunning,
  setProbsToGo,
  setCorrectCount,
  setLastCorrect,
  setLimitCompleted
) where


import Prelude

import Record (set)
import Data.Symbol (SProxy(..))

import Problem


type Model = {
  answer :: String,
  problem :: Problem,
  mode :: GameMode,
  limitRunning :: Boolean,  -- TODO group Limit related stats. Maybe LimitStats perhaps so Main can pass Nothing
  probsToGo :: Int,
  correctCount :: Int,
  lastCorrect :: Boolean,
  limitCompleted :: Boolean
}

data GameMode
  = Endless
  | Limit String  -- maybe make Maybe Int

isLimit :: GameMode -> Boolean
isLimit (Limit _) = true
isLimit _         = false


setAnswer :: String -> Model -> Model
setAnswer = set (SProxy :: SProxy "answer")

setProblem :: Problem -> Model -> Model
setProblem = set (SProxy :: SProxy "problem")

setGameMode :: GameMode -> Model -> Model
setGameMode = set (SProxy :: SProxy "mode")

setLimitRunning :: Boolean -> Model -> Model
setLimitRunning = set (SProxy :: SProxy "limitRunning")

setProbsToGo :: Int -> Model -> Model
setProbsToGo = set (SProxy :: SProxy "probsToGo")

setCorrectCount :: Int -> Model -> Model
setCorrectCount = set (SProxy :: SProxy "correctCount")

setLastCorrect :: Boolean -> Model -> Model
setLastCorrect = set (SProxy :: SProxy "lastCorrect")

setLimitCompleted :: Boolean -> Model -> Model
setLimitCompleted = set (SProxy :: SProxy "limitCompleted")


instance eqGameMode :: Eq GameMode where
  eq Endless   Endless   = true
  eq (Limit x) (Limit y) = x == y
  eq _         _         = false
