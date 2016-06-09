module States exposing (..)

import Types exposing (..)
import AnimationFrame
import Mouse
import Window exposing (..)
import Keyboard exposing (..)
import Config exposing (..)



defaultWorld : ( World, Cmd Msg )
defaultWorld =
    ( { spheres =
            []
      , gravitationalConstant = { size = 0.4 }
      , sphereLimit = { size = 40 }
      , players =
            [ { number = One
              , position = { x = leftLine, y = graphicHeight / 2 }
              , side = Left
              , score = 0
              , size = 10
              , velocity = { x = 0, y = 0 }
              , upAction = NotPressed
              , downAction = NotPressed
              , mass = {size = 10}
              }
            , { number = Two
              , position = { x = rightLine, y = graphicHeight / 2 }
              , side = Right
              , score = 0
              , size = 10
              , velocity = { x = 0, y = 0 }
              , upAction = NotPressed
              , downAction = NotPressed
              , mass = {size = 10}
              }
            ]
            , state = Play
            , graphicSettings = {graphicWidth = graphicWidth, graphicHeight = graphicHeight}
            , leftSideLine = {side = Left, x1 = sideLinePosistion Left, x2 = sideLinePosistion Left, y1 = sideLineStartY, y2 = sideLineEndY}
            , rightSideLine = {side = Right, x1 = sideLinePosistion Left, x2 = sideLinePosistion Left, y1 = sideLineStartY, y2 = sideLineEndY}
      }
    , Cmd.none
    )

subscriptions : World -> Sub Msg
subscriptions world =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , Mouse.clicks Click
        , Window.resizes WindowSize
        ]
