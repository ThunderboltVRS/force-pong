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


type WeaponType
    = Normal
    | Shotgun
    | Field
    | Bouncer


type Side
    = Left
    | Right


type HitType
    = LeftPlayer
    | RightPlayer
    | None


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


type alias Weapon =
    { weaponType : WeaponType
    , force : Force
    }


type alias Sphere =
    { id : String
    , position : Position
    , mass : Mass
    , diameter : Float
    , velocity : Velocity
    , aliveFrames : Float
    , scale : Float
    , merged : Bool
    , hitType : HitType
    }


type alias World =
    { spheres : List (Sphere)
    , players : List (Player)
    , state : State
    , graphicSettings : GraphicSettings
    , leftSideLine : SideLine
    , rightSideLine : SideLine
    , physicsSettings : PhysicsSettings
    , gameSettings : GameSettings
    , innerContainer : Boundary
    , outerContainer : Boundary
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
    , selectedWeapon : WeaponType
    , ammo : Float
    , weapons : List (Weapon)
    , weaponLastFired : Float
    }


type alias GameSettings =
    { maxScore : Float
    , maxShotSize : Float
    , minShotSize : Float
    , shotTimeFactor : Float
    , sphereLimit : Float
    }


type alias GraphicSettings =
    { graphicWidth : Float
    , graphicHeight : Float
    }


type alias PhysicsSettings =
    { gravitationalConstant : Float
    , boundaryDampner : Float
    , maxSphereVelocity : Float
    , maxSphereSize : Float
    }


type alias SideLine =
    { side : Side
    , x1 : Float
    , x2 : Float
    , y1 : Float
    , y2 : Float
    }
