module Config exposing (..)

import Types exposing (..)

playerWidth : number
playerWidth =
    5

playerHeightPercent : number
playerHeightPercent =
    10

containerOffset : number
containerOffset =
    10

containerBorder : number
containerBorder =
    5

sideOffset : number
sideOffset =
    10

graphicWidth : number
graphicWidth =
    1000

graphicHeight : number
graphicHeight =
    600

leftLine : number
leftLine =
    20

rightLine : number
rightLine =
    graphicWidth - leftLine

sideLinePosistion : Side -> Float
sideLinePosistion side =
    case side of
        Left ->
            sideOffset + containerOffset + containerBorder

        Right ->
            graphicWidth - (sideOffset + containerOffset * 2 + containerBorder * 2)

sideLineStartY : number
sideLineStartY =
    containerOffset + containerBorder

sideLineEndY : number
sideLineEndY =
    graphicHeight - (containerOffset + containerBorder * 2)