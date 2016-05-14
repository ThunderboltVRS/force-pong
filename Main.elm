module Main exposing (..)

import Html.App
import Html exposing (..)
import Time exposing (Time, second, millisecond)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame
import Mouse


-- MAIN
main : Program Never
main =
  Html.App.program
    { init = defaultWorld
    , subscriptions = subscriptions
    , update = update
    , view = view
    }
    
-- MODEL
type State = Play | Pause

type alias Input =
  { space : Bool
  , dir1 : Int
  , dir2 : Int
  , delta : Time
  }
  
type alias Position = 
    {
        x : Float,
        y : Float
    }
     
type alias Force =
    {
        magnitudeX : Float,
        magnitudeY : Float
    }
    
type alias Scalar =
    {
        size : Float
    }
     
type alias Mass = 
    {
        size : Float
    }
    
type alias Radius = 
    {
        magnitudeX : Float,
        magnitudeY : Float
    }
    
type alias Velocity =
    {
        magnitudeX : Float,
        magnitudeY : Float
    }

type alias Sphere = 
    { 
        name : String,
        position : Position,
        mass : Mass,
        diameter: Float,
        velocity : Velocity
    }
    
type alias World =
    {
        spheres : List (Sphere)
    }
    
    
-- Update
gravitaionalConstant : Float
gravitaionalConstant = 0.5

update : Msg -> World -> (World, Cmd Msg)
update msg world = 
    case msg of
    Tick dt -> 
     (applyPhysics dt world, Cmd.none)
      
    Click position ->
      (addNewSphere position world, Cmd.none)
      
addNewSphere : Mouse.Position -> World -> World
addNewSphere position world =
     { world | spheres = List.append world.spheres [createSphere (toFloat position.x) (toFloat position.y)] }
     
createSphere : Float -> Float -> Sphere
createSphere x y =
    {
        name = "A",
        position = { x = x, y = y },
        mass = { size = 10 } ,
        diameter = 10,
        velocity = { magnitudeX = 0, magnitudeY = 0 }
    }
      
applyPhysics : Float -> World -> World
applyPhysics dt world =
    { world | spheres = applyForces dt world.spheres
                        |> updatePosistions 
                        |> detectAndMergeCollisions}
    
applyForces : Float -> List(Sphere) -> List(Sphere)
applyForces dt spheres = 
    List.map (\e-> applyGravitationForAll dt e spheres) spheres

updatePosistions : List(Sphere) -> List(Sphere)
updatePosistions spheres = 
    List.map (\e-> updatePosistion e) spheres
    
detectAndMergeCollisions : List(Sphere) -> List(Sphere)
detectAndMergeCollisions spheres = 
    spheres
    
mergeSphere : Sphere -> Sphere -> Sphere
mergeSphere sphereA sphereB = 
    { sphereA | mass = sumMass sphereA.mass sphereB.mass, velocity = sumVelocity sphereA.velocity sphereB.velocity, position =  mergePosition sphereA.position sphereB.position}

sumMass : Mass -> Mass -> Mass
sumMass massA massB = 
    {massA | size = massA.size + massB.size }

sumVelocity : Velocity -> Velocity -> Velocity
sumVelocity velocityA velocityB = 
    { velocityA | magnitudeX = velocityA.magnitudeX + velocityB.magnitudeX, magnitudeY = velocityA.magnitudeY + velocityB.magnitudeY}
    
mergePosition : Position -> Position -> Position
mergePosition positionA positionB = 
    { positionA | x = positionA.x + (positionB.x - positionA.x), y = positionA.y + (positionB.y - positionA.y)}

updatePosistion : Sphere -> Sphere
updatePosistion sphere = 
    { sphere | position = calculateNewPosistion sphere.velocity sphere.position}
    
calculateNewPosistion : Velocity -> Position -> Position
calculateNewPosistion velocity position = 
    {
        x = velocity.magnitudeX + position.x,
        y = velocity.magnitudeY + position.y
    }

filterSpheres : Sphere -> List(Sphere) -> List(Sphere)
filterSpheres sphere spheres =
    List.filter (\e1 -> e1 /=sphere) spheres

applyGravitationForAll : Float -> Sphere -> List(Sphere) -> Sphere
applyGravitationForAll dt sphereA spheres =
    applyForcesToObject sphereA (List.map(\s -> calculateGravitation dt sphereA s) (List.filter(\s -> s /= sphereA) spheres))

calculateGravitation : Float -> Sphere -> Sphere -> Force
calculateGravitation dt sphereA sphereB = 
    let distance = (euclideanDistance sphereA.position sphereB.position)
        force = calculateGravitationScalar sphereA.mass sphereB.mass distance
    in
    if (distance < sphereA.diameter && distance < sphereB.diameter) then emptyForce
    else
    { 
        magnitudeX = dt * force.size * ((sphereA.position.x - sphereB.position.x) / distance),
        magnitudeY = dt * force.size * ((sphereA.position.y - sphereB.position.y) / distance)
    }
    
emptyForce : Force
emptyForce = 
    {
        magnitudeX = 0,
        magnitudeY = 0
    }
        
calculateGravitationScalar : Mass -> Mass -> Float -> Scalar
calculateGravitationScalar mass1 mass2 distance =
    { size = ((mass1.size * mass2.size) / (distance ^ 2)) * -gravitaionalConstant}
    
euclideanDistance : Position -> Position -> Float
euclideanDistance positionA positionB =
    Basics.sqrt ((square (positionB.x - positionA.x)) + (square (positionB.y - positionA.y)))

applyForcesToObject : Sphere -> List(Force) -> Sphere
applyForcesToObject sphere forces = 
    { sphere | velocity = updateVelocityForForces sphere.velocity sphere.mass.size forces }

updateVelocityForForces : Velocity -> Float -> List(Force) -> Velocity
updateVelocityForForces velocity mass forces = 
    { velocity | magnitudeX = velocity.magnitudeX + (sumMagnitudeX(forces) / mass), magnitudeY = velocity.magnitudeY + (sumMagnitudeY(forces) / mass)}
    
sumMagnitudeX : List(Force) -> Float
sumMagnitudeX forces =
    List.foldl sum 0 (List.map (\f -> f.magnitudeX) forces)
    
sumMagnitudeY : List(Force) -> Float
sumMagnitudeY forces =
    List.foldl sum 0 (List.map (\f -> f.magnitudeY) forces)
    
apportionScalarInAxis : Scalar -> Float-> Float -> Float
apportionScalarInAxis force lengthX lengthY =
    force.size * (lengthX / lengthY) * 100

square n = n ^ 2

length a b = b - a

sum a b = a + b

-- STATE
defaultWorld : (World, Cmd Msg) 
defaultWorld = 
 (
     {
        spheres = 
        [
            -- {
            --     name = "A",
            --     position = { x = 100, y = 75 },
            --     mass = { size = 2 } ,
            --     diameter = 2,
            --     velocity = { magnitudeX = 0, magnitudeY = 0 }
            -- }
            -- ,
            -- {
            --     name = "B",
            --     position = { x = 50, y = 50 },
            --     mass = { size = 2 } ,
            --     diameter = 2,
            --     velocity = { magnitudeX = 0, magnitudeY = 0 }
            -- }
            -- ,
            --  {
            --     name = "Unknown",
            --     position = { x = 140, y = 90 },
            --     mass = { size = 2 },
            --     diameter = 2,
            --     velocity = { magnitudeX = 0, magnitudeY = 0 }
            --  }
            --  ,
            -- {
            --     name = "Unknown",
            --     position = { x = 45, y = 30 },
            --     mass = { size = 2 },
            --     diameter = 2,
            --     velocity = { magnitudeX = 0, magnitudeY = 0 }
            -- }
        ]
    }
 ,
 Cmd.none
 )
 
type Msg = Tick Time | Click Mouse.Position
 
subscriptions : World -> Sub Msg
subscriptions world =
  Sub.batch [
      AnimationFrame.diffs Tick,
      Mouse.clicks Click  
  ]

-- VIEW
view : World -> Html Msg
view model =
    Svg.svg [ width "2000px", height "1200px" ] (createCircles model)
      
      
createCircles : World -> List (Svg.Svg msg)
createCircles world = 
    List.map (\e-> createCircle e) world.spheres
    
createCircle : Sphere -> Svg.Svg msg
createCircle sphere = 
    circle [ cx (toString sphere.position.x), cy (toString sphere.position.y), r (toString sphere.diameter), fill "#0B79CE" ] []
