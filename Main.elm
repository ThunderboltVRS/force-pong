module Main exposing (..)

import Html.App
import View exposing (..)
import States exposing (..)
import Update exposing (..)

main : Program Never
main =
  Html.App.program
    { init = defaultWorld
    , subscriptions = subscriptions
    , update = update
    , view = View.view
    }