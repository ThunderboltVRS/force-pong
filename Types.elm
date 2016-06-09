module Types exposing (..)

import Time exposing (Time, second, millisecond)
import Mouse
import Window exposing (..)


type Msg
    = Tick Time
    | Click Mouse.Position
    | WindowSize Window.Size
    | KeyDown Int
    | KeyUp Int


type Side
    = Left
    | Right


type KeyboardKeyAction
    = Pressed
    | NotPressed


type PlayerDirection
    = Up
    | Down


type State
    = Play
    | Pause


type PlayerNumber
    = One
    | Two


type alias Position =
    { x : Float
    , y : Float
    }


type alias Force =
    { x : Float
    , y : Float
    }


type alias Scalar =
    { size : Float
    }


type alias Mass =
    { size : Float
    }


type alias Radius =
    { x : Float
    , y : Float
    }


type alias Velocity =
    { x : Float
    , y : Float
    }


type alias Sphere =
    { id : String
    , position : Position
    , mass : Mass
    , diameter : Float
    , velocity : Velocity
    , aliveFrames : Float
    , merged : Bool
    , scale : Float
    }


type alias World =
    { spheres : List (Sphere)
    , gravitationalConstant : Constant
    , sphereLimit : Constant
    , players : List (Player)
    , state : State
    , graphicSettings : GraphicSettings
    , leftSideLine : SideLine
    , rightSideLine : SideLine
    }


type alias Constant =
    { size : Float
    }


type alias Player =
    { number : PlayerNumber
    , position : Position
    , side : Side
    , score : Float
    , size : Float
    , velocity : Velocity
    , upAction : KeyboardKeyAction
    , downAction : KeyboardKeyAction
    , mass : Mass
    }

type alias GraphicSettings =
    {
        graphicWidth : Float
        , graphicHeight : Float
    }

type alias SideLine =
    {
        side : Side
        , x1 : Float
        , x2 : Float
        , y1 : Float
        , y2 : Float
    }
