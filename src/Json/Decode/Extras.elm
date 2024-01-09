module Json.Decode.Extras exposing
    ( fromMaybe
    , fromResult
    , parseFloat
    , parseInt
    , parseTimePosix
    , parseValue
    , timePosix
    )

import Json.Decode
import Time


fromResult result =
    case result of
        Ok v ->
            Json.Decode.succeed v

        Err v ->
            Json.Decode.fail v


fromMaybe result =
    case result of
        Just v ->
            Json.Decode.succeed v

        Nothing ->
            Json.Decode.fail "Failed to parse int"


parseValue : (String -> Maybe a) -> Json.Decode.Decoder a
parseValue decode =
    Json.Decode.string |> Json.Decode.andThen (decode >> fromMaybe)


parseInt : Json.Decode.Decoder Int
parseInt =
    parseValue String.toInt


parseFloat : Json.Decode.Decoder Float
parseFloat =
    parseValue String.toFloat


timePosix : Json.Decode.Decoder Time.Posix
timePosix =
    Json.Decode.int |> Json.Decode.andThen (Time.millisToPosix >> Json.Decode.succeed)


parseTimePosix : Json.Decode.Decoder Time.Posix
parseTimePosix =
    parseValue (String.toInt >> Maybe.map Time.millisToPosix)
