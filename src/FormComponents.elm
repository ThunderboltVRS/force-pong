module FormComponents exposing (..)

import Material.Scheme
import Material.Options exposing (css)
import Material exposing (..)
import Material.Textfield
import Material.Button
import Material.Grid
import Material.Color
import Material.Toggles
import Material.Options exposing (Style, css, cs)
import Material.Grid exposing (..)
import Html exposing (..)
import Types exposing (..)
import Material.Elevation as Elevation


view : TextFieldSettings -> Mdl -> Html Types.Msg
view settings material =
    div []
        [ Material.Textfield.render MDL
            [ 0 ]
            material
            [ css "margin" "0 24px"
            , Material.Textfield.label settings.label
            , Material.Textfield.floatingLabel
            , Material.Textfield.value settings.text
            ]
        ]


table : List (Html m) -> Material.Grid.Cell m
table contents =
    Material.Grid.cell
        []
        [ Material.Options.div
            [ css "display" "inline-flex"
            , css "flex-direction" "column"
            , css "width" "auto"
            , Elevation.e16
            ]
            contents
        ]


optionsForm : World -> Html Types.Msg
optionsForm model =
    div []
        [ Material.Grid.grid
            []
            [ std [ size All 8, size Tablet 4 ] [ gravitationInput model ]
            , std [ size All 4, size Tablet 2 ] [ pauseButton model ]
            ]
        , Material.Grid.grid
            []
            [ std [ size All 2, size Tablet 2 ] [ gravitySwitch model ]
            ]
        , Material.Grid.grid
            []
            [ std [ size All 8, size Tablet 4 ] [ playerOneName model ]
            ]
        , Material.Grid.grid
            []
            [ std [ size All 8, size Tablet 4 ] [ playerTwoName model ]
            ]
        ]


gravitationInput : World -> Html Types.Msg
gravitationInput model =
    Material.Textfield.render MDL
        [ 0 ]
        model.mdl
        [ Material.Textfield.label "Gravitational Constant"
        , Material.Textfield.floatingLabel
        , Material.Textfield.text'
        , Material.Textfield.onInput GravitationStrength
        , Material.Textfield.value (toString model.physicsSettings.gravitationalConstant)
        ]


playerOneName : World -> Html Types.Msg
playerOneName model =
    Material.Textfield.render MDL
        [ 0 ]
        model.mdl
        [ Material.Textfield.label "Player One Name"
        , Material.Textfield.floatingLabel
        , Material.Textfield.text'
        , Material.Textfield.onInput PlayerOneName
        , Material.Textfield.value (playerName Left model.players)
        ]


playerTwoName : World -> Html Types.Msg
playerTwoName model =
    Material.Textfield.render MDL
        [ 0 ]
        model.mdl
        [ Material.Textfield.label "Player Two Name"
        , Material.Textfield.floatingLabel
        , Material.Textfield.text'
        , Material.Textfield.onInput PlayerTwoName
        , Material.Textfield.value (playerName Right model.players)
        ]


pauseButton : World -> Html Types.Msg
pauseButton model =
    Material.Button.render MDL
        [ 0 ]
        model.mdl
        [ css "margin" "10px 10px"
        , css "float" "right"
        , Material.Button.raised
        , Material.Button.ripple
        , Material.Button.colored
        , Material.Button.onClick TogglePause
        ]
        [ Html.text "Pause" ]


gravitySwitch : World -> Html Types.Msg
gravitySwitch model =
    Material.Toggles.switch MDL
        [ 0 ]
        model.mdl
        [ Material.Toggles.onClick (FlipGravity True)
        , Material.Toggles.ripple
        , Material.Toggles.value (model.physicsSettings.gravityAttractionType == Attract)
        ]
        [ text
            (if (model.physicsSettings.gravityAttractionType == Attract) then
                "Attracting"
             else
                "Repelling"
            )
        ]


style : Int -> List (Style a)
style h =
    [ css "text-sizing" "border-box"
    , css "height" (toString h ++ "px")
    , css "padding-left" "0px"
    , css "padding-top" "0px"
    ]



--Copied in View and here refactor


materialCell : Int -> List (Style a) -> List (Html a) -> Material.Grid.Cell a
materialCell k styling =
    Material.Grid.cell <| List.concat [ style k, styling ]


small : List (Style a) -> List (Html a) -> Material.Grid.Cell a
small =
    materialCell 50


std : List (Style a) -> List (Html a) -> Material.Grid.Cell a
std =
    materialCell (50)



-- Helpers - these may be duplicated in Update.elm, look at how to consolidate


playerName : Side -> List (Player) -> String
playerName side players =
    let
        player =
            findPlayer side players
    in
        case player of
            Nothing ->
                ""

            Just p ->
                p.name


findPlayer : Side -> List (Player) -> Maybe Player
findPlayer side players =
    List.head (List.filter (\e -> e.side == side) players)
