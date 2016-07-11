module FormComponents exposing (..)

import Material.Scheme
import Material.Options exposing (css)
import Material exposing (..)
import Material.Textfield
import Html exposing (..)
import Types exposing (..)

view : TextFieldSettings -> Mdl -> Html Types.Msg
view settings material =
    div []
        [ 
            Material.Textfield.render MDL
            [ 0 ]
            material
            [ css "margin" "0 24px"
            , Material.Textfield.label settings.label
            , Material.Textfield.floatingLabel
            , Material.Textfield.value settings.text
            ]
        ]


