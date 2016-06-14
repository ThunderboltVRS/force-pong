module Update exposing (..)

import Types exposing (..)
import Keyboard exposing (..)
import List.Extra exposing (..)


-- Update


update : Msg -> World -> ( World, Cmd Msg )
update msg world =
    case msg of
        Tick dt ->
            ( applyScoring(applyPhysics dt world), Cmd.none )

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
            -- Left Shoot
            case action of
                Pressed ->
                    attemptShot world Left

                NotPressed ->
                    world

        37 ->
            -- Right shoot
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
    { world | spheres = List.append world.spheres [ createSphere (caclulateShotPosition player) { size = 10 } (caclulateShotVelocity player) ] }


caclulateShotVelocity : Player -> Velocity
caclulateShotVelocity player =
    let
        velocityMultiplierY =
            0.08

        velocityMultiplierX =
            0.4
    in
        { x =
            case player.side of
                Left ->
                    player.mass.size * velocityMultiplierX

                Right ->
                    negate (player.mass.size * velocityMultiplierX)
        , y = player.velocity.y * velocityMultiplierY
        }


caclulateShotPosition : Player -> Position
caclulateShotPosition player =
    { x =
        case player.side of
            Left ->
                player.position.x

            Right ->
                player.position.x
    , y = player.position.y + (player.size / 2)
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
        , scale = scale
        }

applyScoring : World -> World
applyScoring world =
    { world
        | spheres = world.spheres
        , players = world.players
    }


applyPhysics : Float -> World -> World
applyPhysics dt world =
    { world
        | spheres =
            applyForces world.physicsSettings.gravitationalConstant dt world.spheres
                |> applyBoundaryCollisions world
                |> updateSpherePositions
                |> applySphereCollisions
                |> applyPaddleCollisions world
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


applyPaddleCollisions : World -> List (Sphere) -> List (Sphere)
applyPaddleCollisions world spheres =
    List.map (\e -> applyPaddleCollision e world) spheres


applyPaddleCollision : Sphere -> World -> Sphere
applyPaddleCollision sphere world =
    if (collidedWithPaddle world sphere (findPlayer Left world.players)) then
        { sphere | velocity = { x = makePositive sphere.velocity.x, y = sphere.velocity.y } }
        --, position = getPaddleCollidePosition sphere world Left }
    else if (collidedWithPaddle world sphere (findPlayer Right world.players)) then
        { sphere | velocity = { x = makeNegative sphere.velocity.x, y = sphere.velocity.y } }
        --, position = getPaddleCollidePosition sphere world Right }
    else
        sphere


getPaddleCollidePosition : Sphere -> World -> Side -> Position
getPaddleCollidePosition sphere world side =
    -- Don't allow collided sphere to be beyond scoreLine: reset poistion
    case side of
        Left ->
            { x = world.leftSideLine.x2 + sphereRadius sphere, y = sphere.position.y }

        Right ->
            { x = world.rightSideLine.x1 - sphereRadius sphere, y = sphere.position.y }


findPlayer : Side -> List (Player) -> Maybe Player
findPlayer side players =
    List.head (List.filter (\e -> e.side == side) players)


collidedWithPaddle : World -> Sphere -> Maybe Player -> Bool
collidedWithPaddle world sphere player =
    case player of
        Nothing ->
            False

        Just p ->
            case p.side of
                Left ->
                    ((sphere.position.x - sphereRadius sphere) < world.leftSideLine.x2)

                Right ->
                    ((sphere.position.x - sphereRadius sphere) > world.rightSideLine.x1)

applyBoundaryCollisions : World -> List (Sphere) -> List (Sphere)
applyBoundaryCollisions world spheres =
    List.map (\e -> applyVerticalBoundaryCollisions(applyHorizontalBoundaryCollisions e world) world) spheres


applyHorizontalBoundaryCollisions : Sphere -> World -> Sphere
applyHorizontalBoundaryCollisions sphere world =
    if ((sphere.position.y - sphereRadius sphere) < world.leftSideLine.y1) then
        { sphere | velocity = { x = sphere.velocity.x, y = makePositive sphere.velocity.y } }
    else if ((sphere.position.y + sphereRadius sphere) > world.leftSideLine.y2) then
        { sphere | velocity = { x = sphere.velocity.x, y = makeNegative sphere.velocity.y } }
    else
        sphere

applyVerticalBoundaryCollisions : Sphere -> World -> Sphere
applyVerticalBoundaryCollisions sphere world =
    if ((sphere.position.x - sphereRadius sphere) < world.leftSideLine.x2) then
        { sphere | velocity = { x = makePositive sphere.velocity.x, y = sphere.velocity.y } }
    else if ((sphere.position.x + sphereRadius sphere) > world.rightSideLine.x1) then
        { sphere | velocity = { x = makeNegative sphere.velocity.x, y = sphere.velocity.y } }
    else
        sphere


makePositive : Float -> Float
makePositive x =
    abs x


makeNegative : Float -> Float
makeNegative x =
    abs x |> negate


updatePlayerPositions : List (Player) -> List (Player)
updatePlayerPositions players =
    List.map (\e -> updatePlayerPosistion e) players


applySphereCollisions : List (Sphere) -> List (Sphere)
applySphereCollisions spheres =
    filterNothings (List.map mergeSpheres (groupByCollisions spheres))


mergeSphere : Sphere -> Sphere -> Sphere
mergeSphere sphereA sphereB =
    let
        sphereAPost =
            { sphereA | mass = sumMass sphereA.mass sphereB.mass, position = mergePosition sphereA.position sphereB.position }

        sphereBPost =
            { sphereB | mass = { size = 0 }, position = { x = 0, y = 0 }, velocity = { x = 0, y = 0 } }
    in
        updateDiameter sphereA.scale (updateVelocity sphereA sphereB sphereAPost sphereBPost)


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


updateDiameter : Float -> Sphere -> Sphere
updateDiameter scale sphere =
    { sphere | diameter = calculateDiameter scale sphere.mass.size }


calculateDiameter : Float -> Float -> Float
calculateDiameter scale mass =
    mass
        / pi
        |> sqrt
        |> (*) scale


hasCollided : Sphere -> Sphere -> Bool
hasCollided sphereA sphereB =
    euclideanDistance sphereA.position sphereB.position < ((sphereRadius sphereA) + (sphereRadius sphereB))


sphereRadius : Sphere -> Float
sphereRadius sphere =
    sphere.diameter / 2


square : number -> number
square n =
    n ^ 2


mergeSpheres : List (Sphere) -> Maybe Sphere
mergeSpheres spheres =
    let
        lastSphere =
            (List.head (List.reverse spheres))

        -- filter out the last sphere so we don't merge it with itself
        filteredSpheres =
            List.tail (List.reverse spheres)
    in
        if (List.length spheres == 1) then
            lastSphere
        else
            case lastSphere of
                Nothing ->
                    Maybe.Nothing

                Just l ->
                    case filteredSpheres of
                        -- only one sphere: return it
                        Nothing ->
                            Just l

                        -- merge spheres
                        Just f ->
                            Just (List.foldr mergeSphere l f)


groupByCollisions : List (Sphere) -> List (List (Sphere))
groupByCollisions spheres =
    List.Extra.groupWhile (\x y -> hasCollided x y) spheres


filterNothings : List (Maybe a) -> List a
filterNothings list =
    List.filterMap identity list
