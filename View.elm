module View exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (..)
import Types exposing (..)
import Config exposing (..)
import Material.Scheme
import Material.Grid
import Material.Color
import Material.Elevation
import Material.Options exposing (Style, css)
import Material.Grid exposing (..)
import FormComponents exposing (..)


view : World -> Html Msg
view model =
    layoutGrid model
        |> Material.Scheme.topWithScheme Material.Color.Blue Material.Color.LightBlue


layoutGrid : World -> Html Types.Msg
layoutGrid model =
    Material.Grid.grid
        []
        [ std [ size All 8, size Tablet 8, Material.Elevation.e16 ] [ mainDrawingArea model ]
        , std [ size All 3, size Tablet 3, Material.Elevation.e16 ] [ FormComponents.optionsForm model ]
        ]


mainDrawingArea : World -> Svg.Svg msg
mainDrawingArea model =
    Svg.svg
        [ Svg.Attributes.width (toString graphicWidth)
        , Svg.Attributes.height (toString graphicHeight)
        , Svg.Attributes.style "background-color: none"
        ]
        (List.append [ graphicContainer model, sphereGradientColour Left, sphereGradientColour Right ] (renderWorldObjects model))


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
        , strokeOpacity "0.4"
        ]
        []


renderWorldObjects : World -> List (Svg.Svg msg)
renderWorldObjects world =
    renderCircles world
        |> (\e -> List.append e (renderPlayers world.players world))
        |> (\e -> List.append e (renderSideLines world))
        |> (\e -> List.append e (renderScores world))
        |> renderStateMessage world


renderCircles : World -> List (Svg.Svg msg)
renderCircles world =
    List.map (\e -> renderCircle e) world.spheres


renderCircle : Sphere -> Svg.Svg msg
renderCircle sphere =
    circle
        [ cx (toString sphere.position.x)
        , cy (toString sphere.position.y)
        , r (toString (sphere.diameter / 2))
        , fill ("url(#" ++ (sphereGradientName sphere.side) ++ ")")
        ]
        []


renderScores : World -> List (Svg.Svg msg)
renderScores world =
    List.map (\e -> renderScore world e) world.players


renderScore : World -> Player -> Svg.Svg msg
renderScore world player =
    rect
        [ x (outerBorderX player.side world |> toString)
        , y (outerBorderY player.side (scoreHeight world.innerContainer player) world |> toString)
        , width (scoreWidth world |> toString)
        , height (scoreHeight world.innerContainer player |> toString)
        , fill (playerColor player.side)
        , fillOpacity "0.1"
        ]
        []


scoreHeight : Boundary -> Player -> Float
scoreHeight innerContainer player =
    (player.score / 100) * (innerContainer.y2 - innerContainer.y1)


outerBorderX : Side -> World -> Float
outerBorderX side world =
    case side of
        Left ->
            world.outerContainer.x1 + (containerBorder / 2)

        Right ->
            world.outerContainer.x2 - (containerBorder / 2) - scoreWidth world


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


renderSideLines : World -> List (Svg.Svg msg)
renderSideLines world =
    [ renderSideLine world.leftSideLine, renderSideLine world.rightSideLine ]


renderSideLine : SideLine -> Svg.Svg msg
renderSideLine sideline =
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


renderPlayers : List (Player) -> World -> List (Svg.Svg msg)
renderPlayers players world =
    List.map (\e -> renderPlayer e (getSideLine e.side world) world) players


getSideLine : Side -> World -> SideLine
getSideLine side world =
    case side of
        Left ->
            world.leftSideLine

        Right ->
            world.rightSideLine


renderPlayer : Player -> SideLine -> World -> Svg.Svg msg
renderPlayer player sideLine world =
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


renderStateMessage : World -> List (Svg.Svg msg) -> List (Svg.Svg msg)
renderStateMessage world svgs =
    if world.state == Pause then
        List.append [ renderMessage "Paused" world ] svgs
    else
        svgs


renderMessage : String -> World -> Svg.Svg msg
renderMessage message world =
    Svg.text'
        [ x ((toString (world.outerContainer.x2 / 2)) ++ "px")
        , y ((toString (world.outerContainer.y2 / 2)) ++ "px")
        , fill "blue"
        , Svg.Attributes.opacity "0.6"
        ]
        [ Svg.text message
        ]


sphereGradientColour : Side -> Svg.Svg msg
sphereGradientColour side =
    radialGradient
        [ Svg.Attributes.id (sphereGradientName side)
        , cx "50%"
        , cy "50%"
        , r "50%"
        , fx "50%"
        , fy "50%"
        ]
        [ innerSphereGradientColour side, outerSphereGradientColour side ]


sphereGradientName : Side -> String
sphereGradientName side =
    "sphereGradient-" ++ (toString side)


innerSphereGradientColour : Side -> Svg.Svg msg
innerSphereGradientColour side =
    Svg.stop [ Svg.Attributes.offset "0%", stopOpacity "0.3", innerSphereColour side ] []


innerSphereColour : Side -> Svg.Attribute msg
innerSphereColour side =
    case side of
        Left ->
            stopColor "#0B79CE"

        Right ->
            stopColor "red"


outerSphereGradientColour : Side -> Svg.Svg msg
outerSphereGradientColour side =
    Svg.stop [ Svg.Attributes.offset "100%", stopOpacity "1", outerSphereColour side ] []


outerSphereColour : Side -> Svg.Attribute msg
outerSphereColour side =
    case side of
        Left ->
            stopColor "#0B79CE"

        Right ->
            stopColor "red"


playerColor : Side -> String
playerColor side =
    case side of
        Left ->
            "blue"

        Right ->
            "red"



-- Cell styling


style : Int -> List (Style a)
style h =
    [ css "text-sizing" "border-box"
    , css "background-color" "none"
    , css "height" (toString h ++ "px")
    , css "padding-left" "0px"
    , css "padding-top" "0px"
    --, css "color" "white"
    ]



-- Cell variants


materialCell : Int -> List (Style a) -> List (Html a) -> Material.Grid.Cell a
materialCell k styling =
    Material.Grid.cell <| List.concat [ style k, styling ]


small : List (Style a) -> List (Html a) -> Material.Grid.Cell a
small =
    materialCell 50


std : List (Style a) -> List (Html a) -> Material.Grid.Cell a
std =
    materialCell (Config.graphicHeight)
