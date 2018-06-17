module Main exposing (..)

import Html
import View exposing (..)
import States exposing (..)
import Update exposing (..)

main =
    Html.program
        { init = defaultWorld
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
