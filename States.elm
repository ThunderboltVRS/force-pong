module States exposing (..)

import Types exposing (..)
import AnimationFrame
import Mouse
import Window exposing (..)

graphicWidth = 1000
graphicHeight = 600

leftLine = 20
rightLine = graphicWidth - leftLine

defaultWorld : (World, Cmd Msg) 
defaultWorld = 
 (
     {
        spheres = 
            [
                -- {
                --     name = "A",
                --     position = { x = 100, y = 75 },
                --     mass = { size = 2 } ,
                --     diameter = 2,
                --     velocity = { magnitudeX = 0, magnitudeY = 0 }
                -- }
            ]
        ,gravitationalConstant = { size = 0.4}
        ,sphereLimit = { size = 40}
        , players =
            [
                {
                    name = "One"
                    , position = { x = leftLine, y = graphicHeight/2 }
                    , side = Left
                    , score = 0
                    , size = 10
                    , velocity = { magnitudeX = 0, magnitudeY = 0 }
                }
                ,
                {
                    name = "Two"
                    , position = { x = rightLine, y = graphicHeight/2 }
                    , side = Right
                    , score = 0
                    , size = 10
                    , velocity = { magnitudeX = 0, magnitudeY = 0 }
                }
            ]
        , sides =
            [
                Left
                , Right
            ]
    }
 ,
 Cmd.none
 )
 
subscriptions : World -> Sub Msg
subscriptions world =
  Sub.batch [
      AnimationFrame.diffs Tick,
      Mouse.clicks Click,
      Window.resizes WindowSize
  ]