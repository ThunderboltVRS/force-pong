module Update exposing (..)

import Types exposing (..)
import Keyboard exposing (..)


-- Update


update : Msg -> World -> ( World, Cmd Msg )
update msg world =
    case msg of
        Tick dt ->
            ( applyPhysics dt world, Cmd.none )

        Click position ->
            -- ( addNewSphere position world |> limitSpheres, Cmd.none )
            ( world, Cmd.none )

        WindowSize reSize ->
            ( world, Cmd.none )

        KeyDown key ->
            ( applyPlayerKeyChange key Pressed world, Cmd.none )

        KeyUp key ->
            ( applyPlayerKeyChange key NotPressed world, Cmd.none )


applyPlayerKeyChange : KeyCode -> KeyboardKeyAction -> World -> World
applyPlayerKeyChange keyCode action world =
    case keyCode of
        38 ->
            -- Up
            updatePlayerActions world Right action Up

        40 ->
            -- Down
            updatePlayerActions world Right action Down

        87 ->
            -- Up
            updatePlayerActions world Left action Up

        83 ->
            -- Down
            updatePlayerActions world Left action Down

        68 ->
            case action of
                Pressed ->
                    attemptShot world Left
                NotPressed ->
                    world

        37 ->
            case action of
                Pressed ->
                    attemptShot world Right
                NotPressed ->
                    world

        _ ->
            world


updatePlayerActions : World -> Side -> KeyboardKeyAction -> PlayerDirection -> World
updatePlayerActions world side keyboardAction direction =
    { world
        | players =
            List.map
                (\p ->
                    if p.side == side then
                        updatePlayerAction p keyboardAction direction
                    else
                        p
                )
                world.players
    }


updatePlayerAction : Player -> KeyboardKeyAction -> PlayerDirection -> Player
updatePlayerAction player keyboardAction direction =
    case direction of
        Up ->
            { player | upAction = keyboardAction }

        Down ->
            { player | downAction = keyboardAction }


attemptShot : World -> Side -> World
attemptShot world side =
    case
        List.head (List.filter (\p -> p.side == side) world.players)
    of
        Nothing ->
            world

        Just val ->
            shootBall val world


shootBall : Player -> World -> World
shootBall player world =
    { world | spheres = List.append world.spheres [ createSphere { x = player.position.x, y = player.position.y } { size = 10 } (caclulateShotVelocity player) ] }


caclulateShotVelocity : Player -> Velocity
caclulateShotVelocity player =
    let
        velocityX =
            player.mass.size * 0.1
        velocityDampnerY =
             0.1
    in
        { x =
            case player.side of
                Left ->
                    velocityX

                Right ->
                    negate velocityX
        , y = player.velocity.y * velocityDampnerY
        }


createSphere : Position -> Mass -> Velocity -> Sphere
createSphere position initialMass velocity =
    let
        scale =
            5
    in
        { id = "A"
        , position = position
        , mass = initialMass
        , diameter = calculateDiameter scale initialMass.size
        , velocity = velocity
        , aliveFrames = 0
        , merged = False
        , scale = scale
        }


applyPhysics : Float -> World -> World
applyPhysics dt world =
    { world
        | spheres =
            applyForces world.gravitationalConstant dt world.spheres
                |> updateSpherePositions
                |> detectAndMergeCollisions
                |> incrementLifetime
        , players =
            applyPlayerForces dt world.players
                |> updatePlayerPositions
    }


limitSpheres : World -> World
limitSpheres world =
    { world
        | spheres =
            List.sortBy .aliveFrames world.spheres
                |> List.take (round world.sphereLimit.size)
    }


incrementLifetime : List (Sphere) -> List (Sphere)
incrementLifetime spheres =
    List.map (\e -> { e | aliveFrames = e.aliveFrames + 1 }) spheres


applyForces : Constant -> Float -> List (Sphere) -> List (Sphere)
applyForces gravConst dt spheres =
    List.map (\e -> applyGravitationForAll gravConst dt e spheres) spheres


applyPlayerForces : Float -> List (Player) -> List (Player)
applyPlayerForces dt players =
    List.map (\e -> applyPlayerMovementForce dt e) players


applyPlayerMovementForce : Float -> Player -> Player
applyPlayerMovementForce dt player =
    let
        acceleration =
            0.2

        dragPercentage =
            0.3

        maxVelocity =
            10

        upVelocity =
            case player.upAction of
                Pressed ->
                    player.velocity.y - acceleration

                NotPressed ->
                    player.velocity.y * dragPercentage

        downVelocity =
            case player.downAction of
                Pressed ->
                    player.velocity.y + acceleration

                NotPressed ->
                    player.velocity.y * dragPercentage
    in
        { player | velocity = { x = 0, y = clamp (negate maxVelocity) maxVelocity (upVelocity + downVelocity) } }


updateSpherePositions : List (Sphere) -> List (Sphere)
updateSpherePositions spheres =
    List.map (\e -> updateSpherePosistion e) spheres


updatePlayerPositions : List (Player) -> List (Player)
updatePlayerPositions players =
    List.map (\e -> updatePlayerPosistion e) players


detectAndMergeCollisions : List (Sphere) -> List (Sphere)
detectAndMergeCollisions spheres =
    List.map (\e -> mergeSpheres e (List.filter (\e1 -> e1 /= e) spheres)) spheres
        |> removeMergedSpheres


collided : Sphere -> Sphere -> Bool
collided sphereA sphereB =
    let
        distance =
            euclideanDistance sphereA.position sphereB.position
    in
        distance < sphereA.diameter && distance < sphereB.diameter


mergeSphere : Sphere -> Sphere -> Sphere
mergeSphere sphereA sphereB =
    let
        sphereAPost =
            { sphereA | mass = sumMass sphereA.mass sphereB.mass, position = mergePosition sphereA.position sphereB.position }

        sphereBPost =
            { sphereB | mass = { size = 0 }, position = { x = 0, y = 0 }, velocity = { x = 0, y = 0 } }
    in
        updateDiameter sphereA.scale (updateVelocity sphereA sphereB sphereAPost sphereBPost)


markAsMerged : Sphere -> Sphere
markAsMerged sphere =
    { sphere | merged = True }


sumMass : Mass -> Mass -> Mass
sumMass massA massB =
    { massA | size = massA.size + massB.size }


calculateNewVelocity : Sphere -> Sphere -> Sphere -> Sphere -> Velocity
calculateNewVelocity sphereAPrior sphereBPrior sphereAPost sphereBPost =
    { x = calculateNewVelocityInAxis sphereAPrior.mass.size sphereBPrior.mass.size sphereAPrior.velocity.x sphereBPrior.velocity.x sphereAPost.mass.size sphereBPost.mass.size sphereAPost.velocity.x sphereBPost.velocity.x
    , y = calculateNewVelocityInAxis sphereAPrior.mass.size sphereBPrior.mass.size sphereAPrior.velocity.y sphereBPrior.velocity.y sphereAPost.mass.size sphereBPost.mass.size sphereAPost.velocity.y sphereBPost.velocity.y
    }


mergePosition : Position -> Position -> Position
mergePosition positionA positionB =
    { positionA | x = positionA.x + (positionB.x - positionA.x), y = positionA.y + (positionB.y - positionA.y) }


updateSpherePosistion : Sphere -> Sphere
updateSpherePosistion sphere =
    { sphere | position = calculateNewPosistion sphere.velocity sphere.position }


updatePlayerPosistion : Player -> Player
updatePlayerPosistion player =
    { player | position = calculateNewPosistion player.velocity player.position }


updateVelocity : Sphere -> Sphere -> Sphere -> Sphere -> Sphere
updateVelocity sphereAPrior sphereBPrior sphereAPost sphereBPost =
    { sphereAPost | velocity = calculateNewVelocity sphereAPrior sphereBPrior sphereAPost sphereBPost }


calculateMomentum : Float -> Float -> Float
calculateMomentum mass velocity =
    mass * velocity


calculateNewVelocityInAxis : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
calculateNewVelocityInAxis massA massB velocityA velocityB massA' massB' velocityA' velocityB' =
    -- conservation of momentum  (m1 * v1 = m1' * v1')
    let
        momentumBefore =
            (massA * velocityA) + (massB * velocityB)
    in
        momentumBefore / massA'


calculateNewPosistion : Velocity -> Position -> Position
calculateNewPosistion velocity position =
    { x = velocity.x + position.x
    , y = velocity.y + position.y
    }


filterSpheres : Sphere -> List (Sphere) -> List (Sphere)
filterSpheres sphere spheres =
    List.filter (\e1 -> e1 /= sphere) spheres


applyGravitationForAll : Constant -> Float -> Sphere -> List (Sphere) -> Sphere
applyGravitationForAll gravConst dt sphereA spheres =
    applyForcesToObject sphereA (List.map (\s -> calculateGravitation gravConst dt sphereA s) (List.filter (\s -> s /= sphereA) spheres))


calculateGravitation : Constant -> Float -> Sphere -> Sphere -> Force
calculateGravitation gravConst dt sphereA sphereB =
    let
        distance =
            (euclideanDistance sphereA.position sphereB.position)

        force =
            calculateGravitationScalar gravConst sphereA.mass sphereB.mass distance
    in
        if (distance < sphereA.diameter && distance < sphereB.diameter) then
            emptyForce
        else
            { x = dt * force.size * ((sphereA.position.x - sphereB.position.x) / distance)
            , y = dt * force.size * ((sphereA.position.y - sphereB.position.y) / distance)
            }


emptyForce : Force
emptyForce =
    { x = 0
    , y = 0
    }


calculateGravitationScalar : Constant -> Mass -> Mass -> Float -> Scalar
calculateGravitationScalar gravConst mass1 mass2 distance =
    { size = ((mass1.size * mass2.size) / (distance ^ 2)) * (-gravConst.size) }


euclideanDistance : Position -> Position -> Float
euclideanDistance positionA positionB =
    Basics.sqrt ((square (positionB.x - positionA.x)) + (square (positionB.y - positionA.y)))


applyForcesToObject : Sphere -> List (Force) -> Sphere
applyForcesToObject sphere forces =
    { sphere | velocity = updateVelocityForForces sphere.velocity sphere.mass.size forces }


updateVelocityForForces : Velocity -> Float -> List (Force) -> Velocity
updateVelocityForForces velocity mass forces =
    -- F= ma
    { velocity | x = velocity.x + (sumX (forces) / mass), y = velocity.y + (sumY (forces) / mass) }


sumX : List (Force) -> Float
sumX forces =
    List.sum (List.map (\f -> f.x) forces)


sumY : List (Force) -> Float
sumY forces =
    List.sum (List.map (\f -> f.y) forces)


mergeSpheres : Sphere -> List (Sphere) -> Sphere
mergeSpheres testSphere otherSpheres =
    let
        spheresCollided =
            collidedSpheres testSphere otherSpheres
    in
        if List.isEmpty spheresCollided then
            testSphere
        else if (List.any (\f -> f.mass.size > testSphere.mass.size) spheresCollided) || (List.any (\f -> f.aliveFrames > testSphere.aliveFrames && f.mass.size == testSphere.mass.size) spheresCollided) then
            markAsMerged testSphere
        else
            List.foldl mergeSphere testSphere spheresCollided


updateDiameter : Float -> Sphere -> Sphere
updateDiameter scale sphere =
    { sphere | diameter = calculateDiameter scale sphere.mass.size }


calculateDiameter : Float -> Float -> Float
calculateDiameter scale mass =
    mass
        / pi
        |> sqrt
        |> (*) scale


removeMergedSpheres : List (Sphere) -> List (Sphere)
removeMergedSpheres spheres =
    List.filter (\e -> e.merged == False) spheres


collidedSpheres : Sphere -> List (Sphere) -> List (Sphere)
collidedSpheres sphere otherSpheres =
    List.filter (\e -> euclideanDistance sphere.position e.position < (sphere.diameter + e.diameter)) otherSpheres


square : number -> number
square n =
    n ^ 2
