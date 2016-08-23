module JsonUtil exposing (..)

import Types exposing (..)
import States exposing (initialWorld)
import Json.Encode as JsonEncoder
import Json.Decode as JsonDecoder
import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))


-- Encoding


worldEncoder : World -> JsonEncoder.Value
worldEncoder model =
    JsonEncoder.object
        [ ( "physicsSettings", physicsEncoder model.physicsSettings )
        ]


physicsEncoder : PhysicsSettings -> JsonEncoder.Value
physicsEncoder settings =
    JsonEncoder.object
        [ ( "gravitationalConstant", (JsonEncoder.float settings.gravitationalConstant) )
        , ( "boundaryDampner", (JsonEncoder.float settings.boundaryDampner) )
        , ( "maxSphereVelocity", (JsonEncoder.float settings.maxSphereVelocity) )
        , ( "maxSphereSize", (JsonEncoder.float settings.maxSphereSize) )
        , ( "gravityAttractionType", (attactionTypeEncoder settings.gravityAttractionType) )
        , ( "maxGravitationalConstant", (JsonEncoder.float settings.maxGravitationalConstant) )
        , ( "minGravitationalConstant", (JsonEncoder.float settings.minGravitationalConstant) )
        ]


attactionTypeEncoder : AttactionType -> JsonEncoder.Value
attactionTypeEncoder attactionType =
    JsonEncoder.string (toString attactionType)



-- Decoding


physicsDecoder : JsonDecoder.Decoder PhysicsSettings
physicsDecoder =
    JsonDecoder.succeed PhysicsSettings
        |: ("gravitationalConstant" := JsonDecoder.float)
        |: ("boundaryDampner" := JsonDecoder.float)
        |: ("maxSphereVelocity" := JsonDecoder.float)
        |: ("maxSphereSize" := JsonDecoder.float)
        |: (("gravityAttractionType" := JsonDecoder.string) `JsonDecoder.andThen` decodeAttractionType)
        |: ("maxGravitationalConstant" := JsonDecoder.float)
        |: ("minGravitationalConstant" := JsonDecoder.float)


decodeAttractionType : String -> JsonDecoder.Decoder AttactionType
decodeAttractionType input =
    JsonDecoder.succeed
        (case input of
            "Repel" ->
                Attract

            "Attract" ->
                Repel

            _ ->
                Attract
        )


decodePhysics : JsonDecoder.Value -> PhysicsSettings
decodePhysics input =
    case JsonDecoder.decodeValue physicsDecoder input of
        Ok val ->
            val

        Err message ->
            initialWorld.physicsSettings



-- decodeWorld : JsonDecoder.Value -> PhysicsSettings
-- decodePhysics input =
--   case JsonDecoder.decodeValue physicsDecoder input of
--     Ok val -> val
--     Err message -> initialWorld.physicsSettings
