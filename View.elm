module View exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (..)
import Types exposing (..)


graphicWidth = 1000
graphicHeight = 600

view : World -> Html Msg
view model =
    Svg.svg [ Svg.Attributes.width (toString graphicWidth), Svg.Attributes.height (toString graphicHeight), Svg.Attributes.style "background-color: none" ] 
    ( (List.append [sphereGradientColour, graphicContainer model] (createCircles model)))

graphicContainer : World -> Svg.Svg msg
graphicContainer model =
    let offsetX = 10
        offsetY = 10
    in
        Svg.rect [
            Svg.Attributes.x (toString offsetX)
            , Svg.Attributes.y (toString offsetY)
            , Svg.Attributes.width (graphicWidth |> (\a -> a - offsetX * 2) |> toString)
            , Svg.Attributes.height (graphicHeight |> (\a -> a - offsetY * 2) |> toString)
            , Svg.Attributes.fill "none"
            , Svg.Attributes.stroke "blue"
            , Svg.Attributes.strokeWidth "6"
            , Svg.Attributes.strokeOpacity "0.2"
            ] []
      
createCircles : World -> List (Svg.Svg msg)
createCircles world = 
    List.map (\e-> createCircle e) world.spheres
    
createCircle : Sphere -> Svg.Svg msg
createCircle sphere = 
    circle [ cx (toString sphere.position.x), cy (toString sphere.position.y), r (toString sphere.diameter), fill "url(#grad1)" ] [sphereGradientColour]

-- createPlayer : Player -> Svg.Svg msg
-- createPlayer sphere = 
--     circle [ cx (toString sphere.position.x), cy (toString sphere.position.y), r (toString sphere.diameter), fill "url(#grad1)" ] [sphereGradientColour]
    
sphereGradientColour : Svg.Svg msg
sphereGradientColour  =
    radialGradient [Svg.Attributes.id "grad1", cx "50%", cy "50%", r "50%", fx "50%", fy "50%" ] [innerSphereColour, outerSphereColour]

innerSphereColour : Svg.Svg msg
innerSphereColour =
    Svg.stop [offset "0%", stopColor "#0B79CE", stopOpacity "0.3" ] []
    
outerSphereColour : Svg.Svg msg
outerSphereColour =
    Svg.stop [offset "100%", stopColor "#0B79CE", stopOpacity "1" ] []