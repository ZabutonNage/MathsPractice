module Model (
  Model,
  setAnswer
) where


import Record (set)
import Data.Symbol (SProxy(..))

import Problem


type Model = {
  answer :: String,
  problem :: Problem
}

setAnswer :: String -> Model -> Model
setAnswer = set (SProxy :: SProxy "answer")
