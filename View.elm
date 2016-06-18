module View exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (..)
import Types exposing (..)
import Config exposing (..)


view : World -> Html Msg
view model =
    Svg.svg [ Svg.Attributes.width (toString graphicWidth), Svg.Attributes.height (toString graphicHeight), Svg.Attributes.style "background-color: none" ]
        ((List.append [ sphereGradientColour, graphicContainer model ] (createWorldObjects model)))


graphicContainer : World -> Svg.Svg msg
graphicContainer world =
    rect
        [ x (toString world.outerContainer.x1)
        , y (toString world.outerContainer.y1)
        , width ((world.outerContainer.x2 - world.outerContainer.x1) |> toString)
        , height ((world.outerContainer.y2 - world.outerContainer.y1) |> toString)
        , fill "none"
        , stroke "blue"
        , strokeWidth ((toString containerBorder) ++ "px")
        , strokeOpacity "0.2"
        ]
        []


createWorldObjects : World -> List (Svg.Svg msg)
createWorldObjects world =
    createCircles world
        |> (\e -> List.append e (createPlayers world.players world))
        |> (\e -> List.append e (createSideLines world))
        |> (\e -> List.append e (renderScores world))


createCircles : World -> List (Svg.Svg msg)
createCircles world =
    List.map (\e -> createCircle e) world.spheres


createCircle : Sphere -> Svg.Svg msg
createCircle sphere =
    circle [ cx (toString sphere.position.x), cy (toString sphere.position.y), r (toString sphere.diameter), fill "url(#grad1)" ] [ sphereGradientColour ]


renderScores : World -> List (Svg.Svg msg)
renderScores world =
    List.map (\e -> renderScore world e) world.players


renderScore : World -> Player -> Svg.Svg msg
renderScore world player =
    rect
        [ x (outerBorderX player.side world |> toString)
        , y (outerBorderY player.side player.score world |> toString)
        , width (scoreWidth world |> toString)
        , height (toString player.score)
        , fill (playerColor player.side)
        , fillOpacity "0.1"
        ]
        []

outerBorderX : Side -> World -> Float
outerBorderX side world =
    case side of
        Left ->
            world.outerContainer.x1 + (containerBorder / 2)

        Right ->
            world.outerContainer.x2 - (containerBorder / 2) -  scoreWidth world


scoreWidth : World -> Float
scoreWidth world =
    ((world.leftSideLine.x1) - (outerBorderX Left world))


outerBorderY : Side -> Float -> World -> Float
outerBorderY side height world =
    case side of
        Left ->
            world.outerContainer.y2 - (containerBorder / 2) - height

        Right ->
            world.outerContainer.y2 - (containerBorder / 2) - height


createSideLines : World -> List (Svg.Svg msg)
createSideLines world =
    [ createSideLine world.leftSideLine, createSideLine world.rightSideLine ]


createSideLine : SideLine -> Svg.Svg msg
createSideLine sideline =
    line
        [ x1 (toString sideline.x1)
        , x2 (toString sideline.x2)
        , y1 <| (toString sideline.y1)
        , y2 <| (toString sideline.y2)
        , stroke (playerColor sideline.side)
        , strokeWidth "1"
        , strokeOpacity "0.6"
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
        [ x (player.position.x |> toString)
        , y (player.position.y |> toString)
        , width (toString playerWidth)
        , height ((toString player.size))
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





playerColor : Side -> String
playerColor side =
    case side of
        Left ->
            "blue"

        Right ->
            "red"
