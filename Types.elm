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


type Weapon
    = Normal
    | Shotgun
    | Field
    | Bouncer


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


type alias Boundary =
    { x1 : Float
    , x2 : Float
    , y1 : Float
    , y2 : Float
    }


type alias Sphere =
    { id : String
    , position : Position
    , mass : Mass
    , diameter : Float
    , velocity : Velocity
    , aliveFrames : Float
    , scale : Float
    }


type alias World =
    { spheres : List (Sphere)
    , sphereLimit : Constant
    , players : List (Player)
    , state : State
    , graphicSettings : GraphicSettings
    , leftSideLine : SideLine
    , rightSideLine : SideLine
    , physicsSettings : PhysicsSettings
    , outerBoundary : Boundary
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
    , selectedWeapon : Weapon
    , ammo : Float
    }


type alias GraphicSettings =
    { graphicWidth : Float
    , graphicHeight : Float
    }


type alias PhysicsSettings =
    { gravitationalConstant : Constant
    , boundaryDampner : Float
    }


type alias SideLine =
    { side : Side
    , x1 : Float
    , x2 : Float
    , y1 : Float
    , y2 : Float
    }
