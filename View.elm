module View exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (..)
import Types exposing (..)

view : World -> Html Msg
view model =
    let 
        w = "1400px"
        h = "900px"
        -- w = (toString Window.width) ++ "px"
        -- h = (toString Window.height) ++ "px"
    in
    Svg.svg [ Svg.Attributes.width w, Svg.Attributes.height h, Svg.Attributes.style "background-color: lightgray" ] (List.append [sphereGradientColour] (createCircles model))
      
      
createCircles : World -> List (Svg.Svg msg)
createCircles world = 
    List.map (\e-> createCircle e) world.spheres
    
createCircle : Sphere -> Svg.Svg msg
createCircle sphere = 
    circle [ cx (toString sphere.position.x), cy (toString sphere.position.y), r (toString sphere.diameter), fill "url(#grad1)" ] [sphereGradientColour]
    
sphereGradientColour : Svg.Svg msg
sphereGradientColour  =
    radialGradient [Svg.Attributes.id "grad1", cx "50%", cy "50%", r "50%", fx "50%", fy "50%" ] [innerSphereColour, outerSphereColour]

innerSphereColour : Svg.Svg msg
innerSphereColour =
    Svg.stop [offset "0%", stopColor "#0B79CE", stopOpacity "0.3" ] []
    
outerSphereColour : Svg.Svg msg
outerSphereColour =
    Svg.stop [offset "100%", stopColor "#0B79CE", stopOpacity "1" ] []