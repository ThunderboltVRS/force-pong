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
      , sphereLimit = { size = 40 }
      , players =
            [ { number = One
              , position = { x = leftLine, y = graphicHeight / 2 }
              , side = Left
              , score = 100
              , size = 100
              , velocity = { x = 0, y = 0 }
              , upAction = NotPressed
              , downAction = NotPressed
              , mass = { size = 10 }
              , selectedWeapon = Normal
              , ammo = 10
              }
            , { number = Two
              , position = { x = rightLine, y = graphicHeight / 2 }
              , side = Right
              , score = 0
              , size = 100
              , velocity = { x = 0, y = 0 }
              , upAction = NotPressed
              , downAction = NotPressed
              , mass = { size = 10 }
              , selectedWeapon = Normal
              , ammo = 10
              }
            ]
      , state = Play
      , graphicSettings = { graphicWidth = graphicWidth, graphicHeight = graphicHeight }
      , leftSideLine = { side = Left, x1 = sideLinePosistion Left, x2 = sideLinePosistion Left, y1 = sideLineStartY, y2 = sideLineEndY }
      , rightSideLine = { side = Right, x1 = sideLinePosistion Right, x2 = sideLinePosistion Right, y1 = sideLineStartY, y2 = sideLineEndY }
      , physicsSettings = { gravitationalConstant = { size = 0.4 }, boundaryDampner = 0.95 }
      , outerBoundary =
            { x1 = containerBorder + containerOffset + containerBorder
            , x2 = graphicWidth - (containerBorder * 2) - (containerOffset * 2)
            , y1 = containerBorder + containerOffset + containerBorder
            , y2 = graphicHeight - (containerBorder * 2) - (containerOffset * 2)
            }
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
