module Config exposing (..)

import Types exposing (..)


playerWidth : number
playerWidth =
    10


containerOffset : number
containerOffset =
    0


containerBorder : number
containerBorder =
    6


sideOffset : number
sideOffset =
    10


graphicWidth : number
graphicWidth =
    1040


graphicHeight : number
graphicHeight =
    600


sideLineOffset : Float
sideLineOffset =
    40


sideLineWidth : Float
sideLineWidth =
    1


sideLinePosistion : Side -> Float
sideLinePosistion side =
    case side of
        Left ->
            innerContainerX1 + sideLineOffset

        Right ->
            innerContainerX2 - sideLineOffset


innerContainerX1 : Float
innerContainerX1 =
    (containerBorder) + containerOffset


innerContainerX2 : Float
innerContainerX2 =
    graphicWidth - (containerBorder) - (containerOffset)


innerContainerY1 : Float
innerContainerY1 =
    (containerBorder / 2) + containerOffset


innerContainerY2 : Float
innerContainerY2 =
    graphicHeight - (containerBorder / 2) - (containerOffset)


outerContainerX1 : Float
outerContainerX1 =
    containerOffset


outerContainerX2 : Float
outerContainerX2 =
    graphicWidth - containerOffset


outerContainerY1 : Float
outerContainerY1 =
    containerOffset


outerContainerY2 : Float
outerContainerY2 =
    graphicHeight - containerOffset


playerSideLinePosistion : Side -> Float -> Float
playerSideLinePosistion side width =
    case side of
        Left ->
            sideLinePosistion side - width

        Right ->
            sideLinePosistion side
