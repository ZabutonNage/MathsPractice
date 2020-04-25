module Main (
  main
) where


import Prelude
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)

import Flame (QuerySelector(..))
import Flame.Application.NoEffects as FAN

import Data.Maybe (Maybe(Nothing))

import Model
import Update
import View
import Problem


main :: Effect Unit
main = FAN.mount_ (QuerySelector "#main") {
  init,
  update,
  view
}


init :: Model
init = {
  answer: "",
  problem: unsafePerformEffect $ generate Nothing
}
