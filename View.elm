module View exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (..)
import Types exposing (..)
import States exposing (..)

view : World -> Html Msg
view model =
    Svg.svg [ Svg.Attributes.width (toString graphicWidth), Svg.Attributes.height (toString graphicHeight), Svg.Attributes.style "background-color: none" ] 
    ( (List.append [sphereGradientColour, graphicContainer model] (createWorldObjects model)))

playerWidth = 5
playerHeightPercent = 10

containerOffset = 10

leftSideX = 13
rightSideX = 983

graphicContainer : World -> Svg.Svg msg
graphicContainer model =
        rect [
            x (toString containerOffset)
            , y (toString containerOffset)
            , width (graphicWidth |> (\a -> a - containerOffset * 2) |> toString)
            , height (graphicHeight |> (\a -> a - containerOffset * 2) |> toString)
            , fill "none"
            , stroke "blue"
            , strokeWidth "6"
            , strokeOpacity "0.2"
            ] []

createWorldObjects : World -> List (Svg.Svg msg)
createWorldObjects world = 
    createCircles world
    |> (\e ->  List.append e (createPlayers world.players) )

createCircles : World -> List (Svg.Svg msg)
createCircles world = 
    List.map (\e-> createCircle e) world.spheres
    
createCircle : Sphere -> Svg.Svg msg
createCircle sphere = 
    circle [ cx (toString sphere.position.x), cy (toString sphere.position.y), r (toString sphere.diameter), fill "url(#grad1)" ] [sphereGradientColour]

createSideLines : List(Side) -> List (Svg.Svg msg)
createSideLines sides = 
    List.map (\e-> createSideLine e) sides

createSideLine : Side -> Svg.Svg msg
createSideLine side = 
    line [][]

createPlayers : List(Player) -> List (Svg.Svg msg)
createPlayers players = 
    List.map (\e-> createPlayer e) players

createPlayer : Player -> Svg.Svg msg
createPlayer player = 
    rect [ 
        x (sideLinePosistion player.side)
        ,y (player.position.y - 3 |> toString)
        , width (toString playerWidth)
        , height ((toString playerHeightPercent) ++ "%")
        , fill (playerColor player.side)
        , fillOpacity "0.5"
        , stroke (playerColor player.side)
        , strokeWidth "2"
        , strokeOpacity "1"
     ] []
    
sphereGradientColour : Svg.Svg msg
sphereGradientColour  =
    radialGradient [Svg.Attributes.id "grad1", cx "50%", cy "50%", r "50%", fx "50%", fy "50%" ] [innerSphereColour, outerSphereColour]

innerSphereColour : Svg.Svg msg
innerSphereColour =
    Svg.stop [offset "0%", stopColor "#0B79CE", stopOpacity "0.3" ] []
    
outerSphereColour : Svg.Svg msg
outerSphereColour =
    Svg.stop [offset "100%", stopColor "#0B79CE", stopOpacity "1" ] []

sideLinePosistion : Side -> String
sideLinePosistion side =
    case side of
        Left -> toString leftSideX
        Right -> toString rightSideX

playerColor : Side -> String
playerColor side =
    case side of
        Left -> "blue"
        Right -> "red"