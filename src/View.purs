module View (
  view
) where


import Prelude

import Flame (Html)
import Flame.Types (NodeData)
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
      HA.disabled (not model.limitRunning && isLimit model.mode),
      HA.onInput ReadAnswer,
      HA.onKeypress $ whenKey "Enter" \val -> TrySolve val,
      HA.autofocus true
    ]
  ],
  HE.div [ HA.id "mode-settings" ] [
    HE.fieldset [
      HA.class' "radio-group",
      HA.disabled model.limitRunning
--      HA.onInput ChangeGameMode  -- TODO ideally, input listener is here, however, may only listen to radio button changes, not to limit input
    ] [
      HE.div [ HA.class' "mode-endless" ] [
        radioWithLabel "game-mode" "Endless" (model.mode == Endless)
      ],
      HE.div [ HA.class' "mode-limit" ] [
        HE.div_ [
          radioWithLabel "game-mode" "Limit" (isLimit model.mode),
          HE.input ([
            HA.type' "number",
            HA.class' "mode-limit-inp",
            HA.onInput ReadLimit,
            HA.readOnly model.limitRunning  -- TODO disabled would be better but it doesn't seem to work here
          ]
          <> conditionalInputAttrs model.mode)
        ]
      ]
    ],
    limitModeControls model
  ]
]

conditionalInputAttrs :: forall a. GameMode -> Array (NodeData a)
conditionalInputAttrs (Limit lim) = [
  HA.disabled false,
  HA.value lim
  ]
conditionalInputAttrs _ = [
  HA.disabled true,
  HA.value ""
  ]

radioWithLabel :: String -> String -> Boolean -> Html Message
radioWithLabel groupName text checked = HE.label_ [
  HE.input [
    HA.type' "radio",
    HA.name groupName,
    HA.checked checked,
    HA.value text,
    HA.onInput ChangeGameMode
  ],
  HE.text text
  ]

limitModeControls :: Model -> Html Message  -- TODO return array to be concatenated to allow this div being removed from DOM
limitModeControls model = HE.div [
                            HA.class' "mode-limit-controls"
                          ] case model.mode of
                              Limit lim -> controls lim
                              _         -> []
  where
  controls lim = [
    HE.div_ [
      HE.button [
        HA.disabled (not model.limitRunning),
        HA.onClick StopLimit
      ] "Stop",
      HE.button [
        HA.disabled (lim == "" || model.limitRunning),
        HA.onClick StartLimit
      ] "Start"
    ],
    HE.div [ HA.class' "mode-limit-stats", HA.hidden limitStatsHidden ] [
      HE.div_ [
        HE.text "To go: ",
        HE.text (show model.probsToGo)
      ],
      HE.div_ [
        HE.text "Correct: ",
        HE.text (show model.correctCount <> " / " <> lim)
      ]
    ]
  ]
  limitStatsHidden = not (model.limitRunning || model.limitCompleted)

whenKey :: String -> (String -> Message) -> ((Tuple String String) -> Message)
whenKey key f = \(Tuple k v) -> if k /= key
                                  then Ignore
                                  else f v
