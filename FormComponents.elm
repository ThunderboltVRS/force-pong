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
            [ std [ size All 8, size Tablet 1 ] [ gravitationInput model ]
            , std [ size All 4, size Tablet 1 ] [ pauseButton model ]
            ]
            ,
            Material.Grid.grid
            []
            [ std [ size All 12, size Tablet 1 ] [ gravitySwitch model ]
            ]
        ]


gravitationInput : World -> Html Types.Msg
gravitationInput model =
    Material.Textfield.render MDL
        [ 0 ]
        model.mdl
        [ Material.Textfield.label "Gravity Strength"
        , Material.Textfield.floatingLabel
        , Material.Textfield.text'
        , Material.Textfield.onInput GravitationStrength
        , Material.Textfield.value "10"-- (toString model.physicsSettings.gravitationalConstant)
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
            Material.Toggles.switch MDL [0] model.mdl
                [ Material.Toggles.onClick (FlipGravity True)
                , Material.Toggles.ripple
                , Material.Toggles.value (model.physicsSettings.gravityAttractionType == Attract)
                ]
                [ text "Direction" ]


style : Int -> List (Style a)
style h =
    [ css "text-sizing" "border-box"
    , css "background-color" "none"
    , css "height" (toString h ++ "px")
    , css "padding-left" "0px"
    , css "padding-top" "0px"
    -- , css "color" "white"
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
