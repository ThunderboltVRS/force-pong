module Types exposing (..)

import Time exposing (Time, second, millisecond)
import Mouse
import Window exposing (..)

type State = Play | Pause

type Msg = Tick Time | Click Mouse.Position | WindowSize Window.Size

type alias Input =
  { space : Bool
  , dir1 : Int
  , dir2 : Int
  , delta : Time
  }
  
type alias Position = 
    {
        x : Float,
        y : Float
    }
     
type alias Force =
    {
        magnitudeX : Float,
        magnitudeY : Float
    }
    
type alias Scalar =
    {
        size : Float
    }
     
type alias Mass = 
    {
        size : Float
    }
    
type alias Radius = 
    {
        magnitudeX : Float,
        magnitudeY : Float
    }
    
type alias Velocity =
    {
        magnitudeX : Float,
        magnitudeY : Float
    }


type alias Sphere = 
    { 
        id : String,
        position : Position,
        mass : Mass,
        diameter: Float,
        velocity : Velocity,
        aliveFrames : Float,
        merged : Bool
    }
    
type alias World =
    {
        spheres : List (Sphere)
        ,gravitationalConstant : Constant
        ,sphereLimit : Constant
        ,players : List (Player)
        ,sides : List (Side)
    }
    
type alias Constant = 
    {
        size : Float
    }

type alias Player =
    {
        name : String
        , position : Position
        , side : Side
        , score : Float
        , size : Float
        , velocity : Velocity
    }

type Side = Left | Right