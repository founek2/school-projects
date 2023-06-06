module Styles exposing (..)

import Html exposing (Attribute)
import Html.Attributes as Attributes


todoItem : List (Attribute msg)
todoItem =
    [ Attributes.style "padding" "0.3rem"
    ]
