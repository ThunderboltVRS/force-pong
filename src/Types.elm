module Types exposing (..)

import Time exposing (Time, second, millisecond)
import Window exposing (..)
import Material
import Keyboard.Extra exposing (Key(..))


type Msg
    = Tick Time
    | WindowSize Window.Size
    | Mdl (Material.Msg Msg)
    | TogglePause
    | RestartSet
    | NextGame
    | PlayerOneName String
    | PlayerTwoName String
    | FlipGravity Bool
    | Slider Int Float
    | SaveState
    | LoadState
    | KeyboardMsg Keyboard.Extra.Msg


type AttactionType
    = Attract
    | Repel


type WeaponType
    = Normal
    | Reducer


type Side
    = Left
    | Right


type WinState
    = NoWin
    | LeftGameWin
    | RightGameWin
    | LeftOverallWin
    | RightOverallWin


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
    | Win


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
    , side : Side
    , explosive : Bool
    , exploding : Bool
    }


type alias World =
    { spheres : List Sphere
    , players : List Player
    , state : State
    , graphicSettings : GraphicSettings
    , leftSideLine : SideLine
    , rightSideLine : SideLine
    , physicsSettings : PhysicsSettings
    , gameSettings : GameSettings
    , winState : WinState
    , innerContainer : Boundary
    , outerContainer : Boundary
    , mdl : Material.Model
    , pressedKeys : List Key
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
    , weapons : List Weapon
    , weaponLastFired : Float
    , gamesWon : Int
    , name : String
    }


type alias GameSettings =
    { scoreForGame : Float
    , scoreForSet : Float
    , maxShotSize : Float
    , minShotSize : Float
    , shotTimeFactor : Float
    , sphereLimit : Float
    , gamesToWin : Int
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
    , gravityAttractionType : AttactionType
    , maxGravitationalConstant : Float
    , minGravitationalConstant : Float
    }


type alias SideLine =
    { side : Side
    , x1 : Float
    , x2 : Float
    , y1 : Float
    , y2 : Float
    }


type alias Mdl =
    Material.Model


type alias TextFieldSettings =
    { label : String
    , text : String
    }
