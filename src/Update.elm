module Update exposing (..)

import Types exposing (..)
import Keyboard.Extra exposing (Key(..))
import Material


-- Update


update : Msg -> World -> ( World, Cmd Msg )
update msg world =
    case msg of
        Tick dt ->
            case world.state of
                Play ->
                    ( applyScoring (applyPhysics dt world), Cmd.none )

                Types.Pause ->
                    ( world, Cmd.none )

                Win ->
                    ( world, Cmd.none )

        WindowSize reSize ->
            ( world, Cmd.none )

        KeyboardMsg keyMsg ->
            let
                ( pressedKeys, maybeKeyChange ) =
                    Keyboard.Extra.updateWithKeyChange keyMsg world.pressedKeys
            in
                --( { world | pressedKeys = pressedKeys}, Cmd.none )
                ( (applyKeyChanges ({ world | pressedKeys = pressedKeys })), Cmd.none )

        TogglePause ->
            case world.state of
                Play ->
                    ( { world | state = Types.Pause }, Cmd.none )

                Types.Pause ->
                    ( { world | state = Play }, Cmd.none )

                Win ->
                    ( world, Cmd.none )

        -- Do nothing
        NextGame ->
            ( nextGame world, Cmd.none )

        RestartSet ->
            ( restartSet world, Cmd.none )

        PlayerOneName name ->
            ( updatePlayerName name Left world, Cmd.none )

        PlayerTwoName name ->
            ( updatePlayerName name Right world, Cmd.none )

        FlipGravity attracts ->
            ( flipWorldGravity world, Cmd.none )

        Slider a b ->
            case a of
                1 ->
                    ( { world | physicsSettings = updateGravityStrength world.physicsSettings b }, Cmd.none )

                _ ->
                    ( world, Cmd.none )

        SaveState ->
            ( world, Cmd.none )

        LoadState ->
            ( world, Cmd.none )

        Types.Mdl msg ->
            Material.update Mdl msg world


applyKeyChanges : World -> World
applyKeyChanges world =
    updatePlayerActions Right (keyboardAction (List.any (\k -> k == ArrowUp) world.pressedKeys)) Up world
        |> updatePlayerActions Right (keyboardAction (List.any (\k -> k == ArrowDown) world.pressedKeys)) Down
        |> updatePlayerActions Left (keyboardAction (List.any (\k -> k == CharW) world.pressedKeys)) Up
        |> updatePlayerActions Left (keyboardAction (List.any (\k -> k == CharS) world.pressedKeys)) Down
        |> applyShotActions


keyboardAction : Bool -> KeyboardKeyAction
keyboardAction bool =
    if (bool) then
        Pressed
    else
        NotPressed


applyShotActions : World -> World
applyShotActions world =
    applyLeftShotAction world
        |> applyRightShotAction


applyLeftShotAction : World -> World
applyLeftShotAction world =
    if (List.any (\k -> k == ArrowLeft) world.pressedKeys) then
        attemptShot world Right
    else
        world


applyRightShotAction : World -> World
applyRightShotAction world =
    if (List.any (\k -> k == CharD) world.pressedKeys) then
        attemptShot world Left
    else
        world



-- applyPlayerKeyChange : World -> Key -> World
-- applyPlayerKeyChange world key =
--     case key of
--         ArrowLeft ->
--             -- Left Shoot
--             case action of
--                 Pressed ->
--                     attemptShot world Left
--                 NotPressed ->
--                     world
--         37 ->
--             -- Right shoot
--             case action of
--                 Pressed ->
--                     attemptShot world Right
--                 NotPressed ->
--                     world
--         _ ->
--             world


updatePlayerActions : Side -> KeyboardKeyAction -> PlayerDirection -> World -> World
updatePlayerActions side keyboardAction direction world =
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


updatePlayerName : String -> Side -> World -> World
updatePlayerName newName side world =
    { world
        | players =
            List.map
                (\e ->
                    if e.side == side then
                        { e | name = newName }
                    else
                        e
                )
                world.players
    }


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
            0.7

        velocityMultiplierX =
            0.7
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
        , side = player.side
        , position = caclulateShotPosition player ((calculateDiameter scale initialMass.size) / 2)
        , mass = initialMass
        , diameter = calculateDiameter scale initialMass.size
        , velocity = velocity
        , aliveFrames = 0
        , scale = scale
        , merged = False
        , hitType = None
        , explosive = False
        , exploding = False
        }


applyScoring : World -> World
applyScoring world =
    { world | spheres = markScoredSpheres world.spheres world }
        |> updatePlayerScores
        |> removeScoredSpheres
        |> updateWinningPlayers
        |> updateWinState


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


updateWinningPlayers : World -> World
updateWinningPlayers world =
    { world | players = List.map (\e -> updateWinningPlayer e) world.players }


updatePlayerScore : GameSettings -> Player -> List Sphere -> Player
updatePlayerScore settings player spheres =
    { player | score = clamp 0 100 (player.score + playerScoreAsPercentage settings player spheres) }


updateWinningPlayer : Player -> Player
updateWinningPlayer player =
    if player.score == 100 then
        { player | gamesWon = player.gamesWon + 1 }
    else
        player


updateWinState : World -> World
updateWinState world =
    -- if any player won then set world State to Win
    if (anyWinningPlayers world.players) then
        { world
            | state = Win
            , winState =
                case (findWinningPlayer world.players) of
                    Just p ->
                        if (p.gamesWon >= world.gameSettings.gamesToWin) then
                            case p.side of
                                Left ->
                                    LeftOverallWin

                                Right ->
                                    RightOverallWin
                        else
                            case p.side of
                                Left ->
                                    LeftGameWin

                                Right ->
                                    RightGameWin

                    Nothing ->
                        NoWin
        }
    else
        world


playerScoreAsPercentage : GameSettings -> Player -> List Sphere -> Float
playerScoreAsPercentage settings player spheres =
    List.filter (\e -> scored player.side e) spheres
        |> List.map (\e -> e.mass.size)
        |> List.sum
        |> (\e -> toPercentage e settings.scoreForGame)


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


incrementLifetime : List Sphere -> List Sphere
incrementLifetime spheres =
    List.map (\e -> { e | aliveFrames = e.aliveFrames + 1 }) spheres


incrementWeaponLastFired : Float -> List Player -> List Player
incrementWeaponLastFired dt players =
    List.map (\e -> { e | weaponLastFired = e.weaponLastFired + dt }) players


applyForces : PhysicsSettings -> Float -> List Sphere -> List Sphere
applyForces settings dt spheres =
    List.map (\e -> applyGravitationForAll settings dt e spheres) spheres


applyPlayerForces : Float -> List Player -> List Player
applyPlayerForces dt players =
    List.map (\e -> applyPlayerMovementForce dt e) players


applyPlayerMovementForce : Float -> Player -> Player
applyPlayerMovementForce dt player =
    let
        acceleration =
            0.6

        dragPercentage =
            0.3

        maxVelocity =
            15

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


updateSpherePositions : List Sphere -> List Sphere
updateSpherePositions spheres =
    List.map (\e -> updateSpherePosistion e) spheres


applyPaddleCollisions : World -> List Sphere -> List Sphere
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


markScoredSpheres : List Sphere -> World -> List Sphere
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


findPlayer : Side -> List Player -> Maybe Player
findPlayer side players =
    List.head (List.filter (\e -> e.side == side) players)


anyWinningPlayers : List Player -> Bool
anyWinningPlayers players =
    List.any (\e -> winningPlayer e) players


findWinningPlayer : List Player -> Maybe Player
findWinningPlayer players =
    List.filter (\e -> winningPlayer e) players |> List.head


winningPlayer : Player -> Bool
winningPlayer player =
    player.score == 100


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


applyBoundaryCollisions : World -> List Sphere -> List Sphere
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
    makePositive x |> negate


updatePlayerPositions : World -> List Player -> List Player
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


applySphereCollisions : World -> List Sphere -> List Sphere
applySphereCollisions world spheres =
    List.map (\e -> mergeSpheres world e (List.filter (\e1 -> e1 /= e) spheres)) spheres
        |> removeMergedSpheres


largestMassSide : Side -> Side -> Mass -> Mass -> Side
largestMassSide sideA sideB massA massB =
    -- note bias if equal, should be random in that case
    if massA.size > massB.size then
        sideA
    else
        sideB


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
calculateNewVelocityInAxis massA massB velocityA velocityB massA_ massB_ velocityA_ velocityB_ =
    -- conservation of momentum  (m1 * v1 = m1_ * v1_)
    let
        momentumBefore =
            (massA * velocityA) + (massB * velocityB)
    in
        momentumBefore / massA_


calculateNewPosistion : Velocity -> Position -> Position
calculateNewPosistion velocity position =
    { x = velocity.x + position.x
    , y = velocity.y + position.y
    }


filterSpheres : Sphere -> List Sphere -> List Sphere
filterSpheres sphere spheres =
    List.filter (\e1 -> e1 /= sphere) spheres


applyGravitationForAll : PhysicsSettings -> Float -> Sphere -> List Sphere -> Sphere
applyGravitationForAll settings dt sphereA spheres =
    applyForcesToObject settings sphereA (List.map (\s -> calculateGravitation settings dt sphereA s) (List.filter (\s -> s /= sphereA) spheres))


calculateGravitation : PhysicsSettings -> Float -> Sphere -> Sphere -> Force
calculateGravitation settings dt sphereA sphereB =
    let
        distance =
            (euclideanDistance sphereA.position sphereB.position)

        force =
            calculateGravitationScalar settings.gravitationalConstant sphereA.mass sphereB.mass distance

        repel =
            settings.gravityAttractionType == Repel
    in
        if (distance < sphereA.diameter && distance < sphereB.diameter) then
            emptyForce
        else
            { x = dt * force.size * (flipSign repel (apportianForce sphereA.position.x sphereB.position.x distance))
            , y = dt * force.size * (flipSign repel (apportianForce sphereA.position.y sphereB.position.y distance))
            }


apportianForce : Float -> Float -> Float -> Float
apportianForce positionA positionB distance =
    (positionA - positionB) / distance


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
    Basics.sqrt (abs ((square (positionB.x - positionA.x)) + (square (positionB.y - positionA.y))))


applyForcesToObject : PhysicsSettings -> Sphere -> List Force -> Sphere
applyForcesToObject settings sphere forces =
    { sphere | velocity = updateVelocityForForces settings sphere.velocity sphere.mass.size forces }


updateVelocityForForces : PhysicsSettings -> Velocity -> Float -> List Force -> Velocity
updateVelocityForForces settings velocity mass forces =
    -- F= ma
    { velocity
        | x = (velocity.x + (sumX (forces) / mass))
        , y = (velocity.y + (sumY (forces) / mass))
    }


limitSphereVelocity : PhysicsSettings -> Float -> Float
limitSphereVelocity settings velocity =
    clamp (negate settings.maxSphereVelocity) settings.maxSphereVelocity velocity


sumX : List Force -> Float
sumX forces =
    List.sum (List.map (\f -> f.x) forces)


sumY : List Force -> Float
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


mergeSpheres : World -> Sphere -> List Sphere -> Sphere
mergeSpheres world sphere otherSpheres =
    let
        mergableSpheres =
            collidedSpheres sphere otherSpheres
    in
        case mergableSpheres of
            [] ->
                sphere

            spheresToMerge ->
                if (alreadyMerged spheresToMerge sphere) then
                    markAsMerged sphere
                else
                    List.foldl (mergeSphere world) sphere spheresToMerge


mergeSphere : World -> Sphere -> Sphere -> Sphere
mergeSphere world sphereA sphereB =
    let
        sphereAPost =
            { sphereA
                | mass = limitSphereMass world (sumMass sphereA.mass sphereB.mass)
                , position = mergePosition sphereA.position sphereB.position
                , side = largestMassSide sphereA.side sphereB.side sphereA.mass sphereB.mass
                , explosive = world.physicsSettings.maxSphereSize <= sphereA.mass.size
                , exploding = world.physicsSettings.maxSphereSize <= sphereA.mass.size -- Immediate
            }

        sphereBPost =
            { sphereB | mass = { size = 0 }, position = { x = 0, y = 0 }, velocity = { x = 0, y = 0 } }
    in
        updateDiameter sphereA.scale (updateVelocity sphereA sphereB sphereAPost sphereBPost)


alreadyMerged : List Sphere -> Sphere -> Bool
alreadyMerged spheresToMerge sphere =
    (List.any (\f -> f.mass.size > sphere.mass.size) spheresToMerge)
        || (List.any (\f -> f.mass.size == sphere.mass.size && f.aliveFrames > sphere.aliveFrames) spheresToMerge)


collidedSpheres : Sphere -> List Sphere -> List Sphere
collidedSpheres sphere otherSpheres =
    List.filter (\e -> euclideanDistance sphere.position e.position < (sphere.diameter + e.diameter)) otherSpheres


removeMergedSpheres : List Sphere -> List Sphere
removeMergedSpheres spheres =
    List.filter (\e -> e.merged == False) spheres


flipWorldGravity : World -> World
flipWorldGravity world =
    { world | physicsSettings = flipGravity world.physicsSettings }


flipGravity : PhysicsSettings -> PhysicsSettings
flipGravity physicsSettings =
    { physicsSettings | gravityAttractionType = flipAttactionType physicsSettings.gravityAttractionType }


flipAttactionType : AttactionType -> AttactionType
flipAttactionType attactionType =
    if attactionType == Attract then
        Repel
    else
        Attract


flipSign : Bool -> Float -> Float
flipSign flip number =
    if flip then
        negate number
    else
        number


updateGravityStrength : PhysicsSettings -> Float -> PhysicsSettings
updateGravityStrength physicsSettings number =
    { physicsSettings | gravitationalConstant = (clamp 0 60 number) }


nextGame : World -> World
nextGame world =
    { world | state = Play, winState = NoWin, players = List.map (\p -> { p | score = 0 }) world.players }


restartSet : World -> World
restartSet world =
    { world | state = Play, winState = NoWin, players = List.map (\p -> { p | score = 0 }) world.players }
