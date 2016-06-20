module Update exposing (..)

import Types exposing (..)
import Keyboard exposing (..)


-- Update


update : Msg -> World -> ( World, Cmd Msg )
update msg world =
    case msg of
        Tick dt ->
            case world.state of
                Play ->
                    ( applyScoring (applyPhysics dt world), Cmd.none )

                Pause ->
                    ( world, Cmd.none )

        Click position ->
            case world.state of
                Play ->
                    ( { world | state = Pause }, Cmd.none )

                Pause ->
                    ( { world | state = Play }, Cmd.none )

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
    if ((caclulateShotMass world.gameSettings player).size < world.gameSettings.minShotSize) then
        world
    else
        { world
            | spheres = List.append world.spheres [ createSphere player (caclulateShotMass world.gameSettings player) (caclulateShotVelocity player) ]
            , players =
                List.map
                    (\e ->
                        if e.side == player.side then
                            { player | weaponLastFired = 0 }
                        else
                            e
                    )
                    world.players
        }


caclulateShotVelocity : Player -> Velocity
caclulateShotVelocity player =
    let
        velocityMultiplierY =
            0.5

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


caclulateShotMass : GameSettings -> Player -> Mass
caclulateShotMass settings player =
    { size = clamp 0 settings.maxShotSize (player.weaponLastFired * settings.shotTimeFactor) }


caclulateShotPosition : Player -> Float -> Position
caclulateShotPosition player radius =
    { x =
        case player.side of
            Left ->
                player.position.x + radius

            Right ->
                player.position.x - radius
    , y = player.position.y + (player.size / 2)
    }


createSphere : Player -> Mass -> Velocity -> Sphere
createSphere player initialMass velocity =
    let
        scale =
            5
    in
        { id = "A"
        , position = caclulateShotPosition player ((calculateDiameter scale initialMass.size) / 2)
        , mass = initialMass
        , diameter = calculateDiameter scale initialMass.size
        , velocity = velocity
        , aliveFrames = 0
        , scale = scale
        , merged = False
        , hitType = None
        }


applyScoring : World -> World
applyScoring world =
    { world | spheres = markScoredSpheres world.spheres world }
        |> updatePlayerScores
        |> removeScoredSpheres


applyPhysics : Float -> World -> World
applyPhysics dt world =
    { world
        | spheres =
            applyForces world.physicsSettings dt world.spheres
                |> applyBoundaryCollisions world
                |> updateSpherePositions
                |> applySphereCollisions world
                |> applyPaddleCollisions world
                |> incrementLifetime
        , players =
            applyPlayerForces dt world.players
                |> updatePlayerPositions world
                |> incrementWeaponLastFired dt
    }


limitSpheres : World -> World
limitSpheres world =
    { world
        | spheres =
            List.sortBy .aliveFrames world.spheres
                |> List.take (round world.gameSettings.sphereLimit)
    }


updatePlayerScores : World -> World
updatePlayerScores world =
    { world | players = List.map (\e -> updatePlayerScore world.gameSettings e world.spheres) world.players }


updatePlayerScore : GameSettings -> Player -> List (Sphere) -> Player
updatePlayerScore settings player spheres =
    { player | score = clamp 0 100 (player.score + playerScoreAsPercentage settings player spheres) }


playerScoreAsPercentage : GameSettings -> Player -> List (Sphere) -> Float
playerScoreAsPercentage settings player spheres =
    List.filter (\e -> scored player.side e) spheres
        |> List.map (\e -> e.mass.size)
        |> List.sum
        |> (\e -> toPercentage e settings.maxScore)


toPercentage : Float -> Float -> Float
toPercentage numerator denominator =
    numerator / denominator * 100


scored : Side -> Sphere -> Bool
scored side sphere =
    case side of
        Left ->
            sphere.hitType == LeftPlayer

        Right ->
            sphere.hitType == RightPlayer


incrementLifetime : List (Sphere) -> List (Sphere)
incrementLifetime spheres =
    List.map (\e -> { e | aliveFrames = e.aliveFrames + 1 }) spheres


incrementWeaponLastFired : Float -> List (Player) -> List (Player)
incrementWeaponLastFired dt players =
    List.map (\e -> { e | weaponLastFired = e.weaponLastFired + dt }) players


applyForces : PhysicsSettings -> Float -> List (Sphere) -> List (Sphere)
applyForces settings dt spheres =
    List.map (\e -> applyGravitationForAll settings dt e spheres) spheres


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
        { sphere
            | velocity = { x = makePositive sphere.velocity.x, y = sphere.velocity.y }
            , position = sphereDefaultPosition sphere Left world
        }
    else if (collidedWithPaddle world sphere (findPlayer Right world.players)) then
        { sphere
            | velocity = { x = makeNegative sphere.velocity.x, y = sphere.velocity.y }
            , position = sphereDefaultPosition sphere Right world
        }
    else
        sphere


removeScoredSpheres : World -> World
removeScoredSpheres world =
    { world | spheres = List.filter (\e -> e.hitType == None) world.spheres }


markScoredSpheres : List (Sphere) -> World -> List (Sphere)
markScoredSpheres spheres world =
    List.map (\e -> markScoredSphere e world) spheres


markScoredSphere : Sphere -> World -> Sphere
markScoredSphere sphere world =
    if (sphereBeyondScoreLine Left sphere world) then
        { sphere | hitType = LeftPlayer }
    else if (sphereBeyondScoreLine Right sphere world) then
        { sphere | hitType = RightPlayer }
    else
        sphere


sphereBeyondScoreLine : Side -> Sphere -> World -> Bool
sphereBeyondScoreLine side sphere world =
    case side of
        Left ->
            ((sphere.position.x - sphereRadius sphere) < world.leftSideLine.x2)

        Right ->
            ((sphere.position.x + sphereRadius sphere) > world.rightSideLine.x1)


sphereDefaultPosition : Sphere -> Side -> World -> Position
sphereDefaultPosition sphere side world =
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
    let
        radius =
            sphereRadius sphere
    in
        case player of
            Nothing ->
                False

            Just p ->
                case p.side of
                    Left ->
                        ((sphere.position.x - radius) < world.leftSideLine.x2)
                            && ((sphere.position.y) < p.position.y + p.size && (sphere.position.y) > p.position.y)

                    Right ->
                        ((sphere.position.x + radius) > world.rightSideLine.x1)
                            && ((sphere.position.y) < p.position.y + p.size && (sphere.position.y) > p.position.y)


applyBoundaryCollisions : World -> List (Sphere) -> List (Sphere)
applyBoundaryCollisions world spheres =
    List.map (\e -> applyHorizontalBoundaryCollisions e world) spheres


applyHorizontalBoundaryCollisions : Sphere -> World -> Sphere
applyHorizontalBoundaryCollisions sphere world =
    if ((sphere.position.y - sphereRadius sphere) < world.leftSideLine.y1) then
        { sphere | velocity = { x = sphere.velocity.x, y = makePositive sphere.velocity.y } }
    else if ((sphere.position.y + sphereRadius sphere) > world.leftSideLine.y2) then
        { sphere | velocity = { x = sphere.velocity.x, y = makeNegative sphere.velocity.y } }
    else
        sphere


makePositive : Float -> Float
makePositive x =
    abs x


makeNegative : Float -> Float
makeNegative x =
    abs x |> negate


updatePlayerPositions : World -> List (Player) -> List (Player)
updatePlayerPositions world players =
    List.map (\e -> limitPlayerBoundary world e) players
        |> List.map (\e -> updatePlayerPosistion e)


limitPlayerBoundary : World -> Player -> Player
limitPlayerBoundary world player =
    if playerOutOfBounds world player then
        { player
            | velocity = { x = 0, y = 0 }
            , position =
                { x = player.position.x
                , y =
                    if player.position.y > world.innerContainer.y2 - player.size then
                        world.innerContainer.y2 - player.size
                    else
                        world.innerContainer.y1
                }
        }
    else
        player


playerOutOfBounds : World -> Player -> Bool
playerOutOfBounds world player =
    player.position.y
        < world.innerContainer.y1
        || (player.position.y + player.size)
        > world.innerContainer.y2


applySphereCollisions : World -> List (Sphere) -> List (Sphere)
applySphereCollisions world spheres =
    List.map (\e -> mergeSpheres world e (List.filter (\e1 -> e1 /= e) spheres)) spheres
        |> removeMergedSpheres


mergeSphere : World -> Sphere -> Sphere -> Sphere
mergeSphere world sphereA sphereB =
    let
        sphereAPost =
            { sphereA | mass = limitSphereMass world (sumMass sphereA.mass sphereB.mass), position = mergePosition sphereA.position sphereB.position }

        sphereBPost =
            { sphereB | mass = { size = 0 }, position = { x = 0, y = 0 }, velocity = { x = 0, y = 0 } }
    in
        updateDiameter sphereA.scale (updateVelocity sphereA sphereB sphereAPost sphereBPost)


limitSphereMass : World -> Mass -> Mass
limitSphereMass world mass =
    { mass | size = clamp 0 world.physicsSettings.maxSphereSize mass.size }


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


applyGravitationForAll : PhysicsSettings -> Float -> Sphere -> List (Sphere) -> Sphere
applyGravitationForAll settings dt sphereA spheres =
    applyForcesToObject settings sphereA (List.map (\s -> calculateGravitation settings dt sphereA s) (List.filter (\s -> s /= sphereA) spheres))


calculateGravitation : PhysicsSettings -> Float -> Sphere -> Sphere -> Force
calculateGravitation settings dt sphereA sphereB =
    let
        distance =
            (euclideanDistance sphereA.position sphereB.position)

        force =
            calculateGravitationScalar settings.gravitationalConstant sphereA.mass sphereB.mass distance
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


calculateGravitationScalar : Float -> Mass -> Mass -> Float -> Scalar
calculateGravitationScalar gravConst mass1 mass2 distance =
    { size = ((mass1.size * mass2.size) / (distance ^ 2)) * (-gravConst) }


euclideanDistance : Position -> Position -> Float
euclideanDistance positionA positionB =
    Basics.sqrt ((square (positionB.x - positionA.x)) + (square (positionB.y - positionA.y)))


applyForcesToObject : PhysicsSettings -> Sphere -> List (Force) -> Sphere
applyForcesToObject settings sphere forces =
    { sphere | velocity = updateVelocityForForces settings sphere.velocity sphere.mass.size forces }


updateVelocityForForces : PhysicsSettings -> Velocity -> Float -> List (Force) -> Velocity
updateVelocityForForces settings velocity mass forces =
    -- F= ma
    { velocity
        | x = (velocity.x + (sumX (forces) / mass))
        , y = (velocity.y + (sumY (forces) / mass))
    }


limitSphereVelocity : PhysicsSettings -> Float -> Float
limitSphereVelocity settings velocity =
    clamp (negate settings.maxSphereVelocity) settings.maxSphereVelocity velocity


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
    (<) (euclideanDistance sphereA.position sphereB.position) ((sphereRadius sphereA) + (sphereRadius sphereB))


sphereRadius : Sphere -> Float
sphereRadius sphere =
    sphere.diameter / 2


square : number -> number
square n =
    n ^ 2


filterNothings : List (Maybe a) -> List a
filterNothings list =
    List.filterMap identity list


groupFunction : (a -> b -> a) -> List a -> List a
groupFunction func objects =
    objects


markAsMerged : Sphere -> Sphere
markAsMerged sphere =
    { sphere | merged = True }


mergeSpheres : World -> Sphere -> List (Sphere) -> Sphere
mergeSpheres world testSphere otherSpheres =
    let
        spheresCollided =
            collidedSpheres testSphere otherSpheres
    in
        if List.isEmpty spheresCollided then
            testSphere
        else if
            (List.any (\f -> f.mass.size > testSphere.mass.size) spheresCollided)
                || (List.any (\f -> f.mass.size == testSphere.mass.size && f.aliveFrames > testSphere.aliveFrames) spheresCollided)
        then
            markAsMerged testSphere
        else
            List.foldl (mergeSphere world) testSphere spheresCollided


collidedSpheres : Sphere -> List (Sphere) -> List (Sphere)
collidedSpheres sphere otherSpheres =
    List.filter (\e -> euclideanDistance sphere.position e.position < (sphere.diameter + e.diameter)) otherSpheres


removeMergedSpheres : List (Sphere) -> List (Sphere)
removeMergedSpheres spheres =
    List.filter (\e -> e.merged == False) spheres
