module View exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (..)
import Types exposing (..)
import States exposing (..)
import Config exposing (..)


view : World -> Html Msg
view model =
    Svg.svg [ Svg.Attributes.width (toString graphicWidth), Svg.Attributes.height (toString graphicHeight), Svg.Attributes.style "background-color: none" ]
        ((List.append [ sphereGradientColour, graphicContainer model ] (createWorldObjects model)))


graphicContainer : World -> Svg.Svg msg
graphicContainer model =
    rect
        [ x (toString containerOffset)
        , y (toString containerOffset)
        , width (graphicWidth |> (\a -> a - containerOffset * 2 - containerBorder * 2) |> toString)
        , height (graphicHeight |> (\a -> a - containerOffset * 2 - containerBorder * 2) |> toString)
        , fill "none"
        , stroke "blue"
        , strokeWidth (toString containerBorder)
        , strokeOpacity "0.2"
        ]
        []


createWorldObjects : World -> List (Svg.Svg msg)
createWorldObjects world =
    createCircles world
        |> (\e -> List.append e (createPlayers world.players world))
        |> (\e -> List.append e (createSideLines world))


createCircles : World -> List (Svg.Svg msg)
createCircles world =
    List.map (\e -> createCircle e) world.spheres


createCircle : Sphere -> Svg.Svg msg
createCircle sphere =
    circle [ cx (toString sphere.position.x), cy (toString sphere.position.y), r (toString sphere.diameter), fill "url(#grad1)" ] [ sphereGradientColour ]


createSideLines : World -> List (Svg.Svg msg)
createSideLines world =
    [ createSideLine world.leftSideLine, createSideLine world.leftSideLine ]


createSideLine : SideLine -> Svg.Svg msg
createSideLine sideline =
    line
        [ x1 (toString sideline.x1)
        , x2 (toString sideline.x2)
        , y1 <| (toString sideline.y1)
        , y2 <| (toString sideline.y2)
        , stroke (playerColor sideline.side)
        , strokeWidth "1"
        , strokeOpacity "0.1"
        ]
        []


createPlayers : List (Player) -> World -> List (Svg.Svg msg)
createPlayers players world =
    List.map (\e -> createPlayer e (getSideLine e.side world) world) players


getSideLine : Side -> World -> SideLine
getSideLine side world =
    case side of
        Left ->
            world.leftSideLine

        Right ->
            world.rightSideLine


createPlayer : Player -> SideLine -> World -> Svg.Svg msg
createPlayer player sideLine world =
    rect
        [ x ((playerSideLinePosistion player.side playerWidth) |> toString)
        , y (player.position.y |> toString)
        , width (toString playerWidth)
        , height ((toString playerHeightPercent) ++ "%")
        , fill (playerColor player.side)
        , fillOpacity "0.5"
        , stroke (playerColor player.side)
        , strokeWidth "2"
        , strokeOpacity "1"
        ]
        []


sphereGradientColour : Svg.Svg msg
sphereGradientColour =
    radialGradient [ Svg.Attributes.id "grad1", cx "50%", cy "50%", r "50%", fx "50%", fy "50%" ] [ innerSphereColour, outerSphereColour ]


innerSphereColour : Svg.Svg msg
innerSphereColour =
    Svg.stop [ offset "0%", stopColor "#0B79CE", stopOpacity "0.3" ] []


outerSphereColour : Svg.Svg msg
outerSphereColour =
    Svg.stop [ offset "100%", stopColor "#0B79CE", stopOpacity "1" ] []


playerSideLinePosistion : Side -> Float -> Float
playerSideLinePosistion side width =
    case side of
        Left ->
            (sideLinePosistion side) - width

        Right ->
            sideLinePosistion side


playerColor : Side -> String
playerColor side =
    case side of
        Left ->
            "blue"

        Right ->
            "red"
