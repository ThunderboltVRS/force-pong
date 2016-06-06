module Main exposing (..)

import Html.App

import Time exposing (Time, second, millisecond)


import AnimationFrame
import Mouse
import Window exposing (..)
import Types exposing (..)
import View exposing (..)


-- MAIN
main : Program Never
main =
  Html.App.program
    { init = defaultWorld
    , subscriptions = subscriptions
    , update = update
    , view = View.view
    }
      
-- Update

update : Msg -> World -> (World, Cmd Msg)
update msg world = 
    case msg of
    Tick dt -> 
     (applyPhysics dt world, Cmd.none)
      
    Click position ->
      (addNewSphere position world |> limitSpheres, Cmd.none)
      
    WindowSize reSize ->
        (world, Cmd.none)
            
      
addNewSphere : Mouse.Position -> World -> World
addNewSphere position world =
     { world | spheres = List.append world.spheres [createSphere (toFloat position.x) (toFloat position.y)] }
     
createSphere : Float -> Float -> Sphere
createSphere x y =
    let initialMassSize = 10
    in
    {
        id = "A",
        position = { x = x, y = y },
        mass = { size = initialMassSize } ,
        diameter = calculateDiamter initialMassSize,
        velocity = { magnitudeX = 0, magnitudeY = 0 },
        aliveFrames = 0,
        merged = False
    }
      
applyPhysics : Float -> World -> World
applyPhysics dt world =
    { world | spheres = applyForces world.gravitationalConstant dt world.spheres
                        |> updatePosistions 
                        |> detectAndMergeCollisions
                        |> incrementLifetime
                        }
                        
                        
limitSpheres : World -> World
limitSpheres world = 
    { world | spheres = List.sortBy .aliveFrames world.spheres
                |> List.take (round world.sphereLimit.size)
    }
        
incrementLifetime : List(Sphere) -> List(Sphere)
incrementLifetime spheres = 
    List.map (\e-> { e | aliveFrames = e.aliveFrames + 1}) spheres
    
applyForces : Constant -> Float -> List(Sphere) -> List(Sphere)
applyForces gravConst dt spheres = 
    List.map (\e-> applyGravitationForAll gravConst dt e spheres) spheres

updatePosistions : List(Sphere) -> List(Sphere)
updatePosistions spheres = 
    List.map (\e-> updatePosistion e) spheres
    
detectAndMergeCollisions : List(Sphere) -> List(Sphere)
detectAndMergeCollisions spheres = 
    List.map (\e-> mergeSpheres e (List.filter (\e1 -> e1 /= e) spheres)) spheres
    |> removeMergedSpheres
        
collided : Sphere -> Sphere -> Bool
collided sphereA sphereB =
    let distance = euclideanDistance sphereA.position sphereB.position
    in 
        distance < sphereA.diameter && distance < sphereB.diameter
    
mergeSphere : Sphere -> Sphere -> Sphere
mergeSphere sphereA sphereB = 
    let sphereAPost = { sphereA | mass = sumMass sphereA.mass sphereB.mass, position =  mergePosition sphereA.position sphereB.position}
        sphereBPost = { sphereB | mass = {size = 0}, position =  mergePosition sphereA.position sphereB.position, velocity = {magnitudeX = 0, magnitudeY = 0}}
    in
        (updateVelocity sphereA sphereB sphereAPost sphereBPost)
        |> updateDiameter
    
markAsMerged : Sphere -> Sphere
markAsMerged sphere = 
    { sphere | merged = True}

sumMass : Mass -> Mass -> Mass
sumMass massA massB = 
    {massA | size = massA.size + massB.size }

calculateNewVelocity : Sphere -> Sphere -> Sphere -> Sphere -> Velocity
calculateNewVelocity sphereAPrior sphereBPrior sphereAPost sphereBPost = 
    { 
        magnitudeX = calculateNewVelocityInAxis sphereAPrior.mass.size sphereBPrior.mass.size sphereAPrior.velocity.magnitudeX sphereBPrior.velocity.magnitudeX sphereAPost.mass.size sphereBPost.mass.size sphereAPost.velocity.magnitudeX sphereBPost.velocity.magnitudeX, 
        magnitudeY = calculateNewVelocityInAxis sphereAPrior.mass.size sphereBPrior.mass.size sphereAPrior.velocity.magnitudeY sphereBPrior.velocity.magnitudeY sphereAPost.mass.size sphereBPost.mass.size sphereAPost.velocity.magnitudeY sphereBPost.velocity.magnitudeY
    }
    
mergePosition : Position -> Position -> Position
mergePosition positionA positionB = 
    { positionA | x = positionA.x + (positionB.x - positionA.x), y = positionA.y + (positionB.y - positionA.y)}

updatePosistion : Sphere -> Sphere
updatePosistion sphere = 
    { sphere | position = calculateNewPosistion sphere.velocity sphere.position}
    
updateVelocity : Sphere -> Sphere -> Sphere -> Sphere -> Sphere
updateVelocity sphereAPrior sphereBPrior sphereAPost sphereBPost =
    { sphereAPost | velocity = calculateNewVelocity sphereAPrior sphereBPrior sphereAPost sphereBPost }    

calculateMomentum : Float -> Float -> Float
calculateMomentum mass velocity =
    mass * velocity

-- conservation of momentum  (m1 * v1 = m1' * v1')
calculateNewVelocityInAxis : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
calculateNewVelocityInAxis massA massB velocityA velocityB massA' massB' velocityA' velocityB' =
    let momentumBefore = (massA * velocityA) + (massB * velocityB)
    in 
        momentumBefore / massA'
        
calculateNewPosistion : Velocity -> Position -> Position
calculateNewPosistion velocity position = 
    {
        x = velocity.magnitudeX + position.x,
        y = velocity.magnitudeY + position.y
    }

filterSpheres : Sphere -> List(Sphere) -> List(Sphere)
filterSpheres sphere spheres =
    List.filter (\e1 -> e1 /=sphere) spheres

applyGravitationForAll : Constant -> Float -> Sphere -> List(Sphere) -> Sphere
applyGravitationForAll gravConst dt sphereA spheres =
    applyForcesToObject sphereA (List.map(\s -> calculateGravitation gravConst dt sphereA s) (List.filter(\s -> s /= sphereA) spheres))

calculateGravitation : Constant -> Float -> Sphere -> Sphere -> Force
calculateGravitation gravConst dt sphereA sphereB = 
    let distance = (euclideanDistance sphereA.position sphereB.position)
        force = calculateGravitationScalar gravConst sphereA.mass sphereB.mass distance
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

calculateGravitationScalar : Constant -> Mass -> Mass -> Float -> Scalar
calculateGravitationScalar gravConst mass1 mass2 distance =
    { size = ((mass1.size * mass2.size) / (distance ^ 2)) * (-gravConst.size)}
    
euclideanDistance : Position -> Position -> Float
euclideanDistance positionA positionB =
    Basics.sqrt ((square (positionB.x - positionA.x)) + (square (positionB.y - positionA.y)))

applyForcesToObject : Sphere -> List(Force) -> Sphere
applyForcesToObject sphere forces = 
    { sphere | velocity = updateVelocityForForces sphere.velocity sphere.mass.size forces }

-- F= ma
updateVelocityForForces : Velocity -> Float -> List(Force) -> Velocity
updateVelocityForForces velocity mass forces = 
    { velocity | magnitudeX = velocity.magnitudeX + (sumMagnitudeX(forces) / mass), magnitudeY = velocity.magnitudeY + (sumMagnitudeY(forces) / mass)}
    
sumMagnitudeX : List(Force) -> Float
sumMagnitudeX forces =
    List.foldl sum 0 (List.map (\f -> f.magnitudeX) forces)
    
sumMagnitudeY : List(Force) -> Float
sumMagnitudeY forces =
    List.foldl sum 0 (List.map (\f -> f.magnitudeY) forces)
    
mergeSpheres : Sphere -> List(Sphere) -> Sphere
mergeSpheres testSphere otherSpheres =
    let spheresCollided = collidedSpheres testSphere otherSpheres
    in 
        if List.isEmpty spheresCollided then
            testSphere 
        else
            if (List.any (\f -> f.mass.size > testSphere.mass.size) spheresCollided) || (List.any (\f -> f.aliveFrames > testSphere.aliveFrames) spheresCollided) then
                markAsMerged testSphere
            else
                List.foldl mergeSphere testSphere spheresCollided
                
updateDiameter : Sphere -> Sphere
updateDiameter sphere =
    { sphere | diameter = calculateDiamter sphere.mass.size }

calculateDiamter : Float -> Float
calculateDiamter mass =
    let scale = 5
    in
        mass / pi
        |> sqrt
        |> (*) scale     
            
removeMergedSpheres : List(Sphere) -> List(Sphere)
removeMergedSpheres spheres =
    List.filter (\e -> e.merged == False) spheres
    
collidedSpheres : Sphere -> List(Sphere) -> List(Sphere) 
collidedSpheres sphere otherSpheres =
    List.filter (\e-> euclideanDistance sphere.position e.position < (sphere.diameter + e.diameter)) otherSpheres

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
            ]
        ,gravitationalConstant = { size = 0.4}
        ,sphereLimit = { size = 40}
        , players =
            [
                {
                    name = "One"
                    , position = { x = 250, y = 0 }
                    , side = Left
                    , score = 0
                    , size = 10
                    , velocity = { magnitudeX = 0, magnitudeY = 0 }
                }
                ,
                {
                    name = "Two"
                    , position = { x = 250, y = 0 }
                    , side = Left
                    , score = 0
                    , size = 10
                    , velocity = { magnitudeX = 0, magnitudeY = 0 }
                }
            ]
    }
 ,
 Cmd.none
 )
 
subscriptions : World -> Sub Msg
subscriptions world =
  Sub.batch [
      AnimationFrame.diffs Tick,
      Mouse.clicks Click,
      Window.resizes WindowSize
  ]