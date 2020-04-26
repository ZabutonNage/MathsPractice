module View (
  view
) where


import Prelude

import Flame (Html)
import Flame.HTML.Element as HE
import Flame.HTML.Attribute as HA

import Data.Tuple (Tuple(..))

import Model
import Message


view :: Model -> Html Message
view model = HE.main "main" [
  HE.div [HA.id "prob"] $ show model.problem,
  HE.div [HA.id "answer"] [
    HE.span_ "x = ",
    HE.input [
      HA.type' "number",
      HA.value model.answer,
      HA.onInput ReadInput,
      HA.onKeypress $ whenKey "Enter" \val -> TrySolve val,
      HA.autofocus true
    ]
  ]
]


whenKey :: String -> (String -> Message) -> ((Tuple String String) -> Message)
whenKey key f = \(Tuple k v) -> if k /= key
                                  then Ignore
                                  else f v
