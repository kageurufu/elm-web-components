module Timer exposing (main, timer)

import Browser.Events
import Components
import Html exposing (Html)
import Html.Attributes
import Iso8601
import Json.Decode
import Json.Decode.Extras
import Task
import Time


timer : Int -> Html msg
timer millis =
    Components.node "elm-timer"
        [ Html.Attributes.attribute "datetime" (String.fromInt millis) ]


main : Components.Component Model Msg
main =
    Components.component
        { decodeFlags = decodeFlags
        , init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , attributeUpdate = FlagChange
        }


type alias Flags =
    { time : Time.Posix }


type alias Model =
    { time : Time.Posix, currentTime : Time.Posix }


type Msg
    = FlagChange Flags
    | Tick Time.Posix


decodeFlags : Json.Decode.Decoder Flags
decodeFlags =
    Json.Decode.map Flags
        (Json.Decode.field "datetime"
            (Json.Decode.oneOf
                [ Json.Decode.Extras.timePosix
                , Json.Decode.Extras.parseTimePosix
                ]
            )
        )


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { time = flags.time, currentTime = Time.millisToPosix 0 }
    , Task.perform Tick Time.now
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        FlagChange flags ->
            ( { model | time = flags.time }, Cmd.none )

        Tick ts ->
            ( { model | currentTime = ts }, Cmd.none )


view : Model -> Html msg
view { time, currentTime } =
    Html.time
        [ Html.Attributes.datetime <| formatTime time ]
        [ Html.text <| (timeDiff currentTime time |> formatTimeDiff) ]


timeDiff : Time.Posix -> Time.Posix -> Int
timeDiff start end =
    Time.posixToMillis end - Time.posixToMillis start


formatTimeDiff : Int -> String
formatTimeDiff millis =
    let
        ms =
            abs millis

        future =
            millis > 0

        ( scale, suffix ) =
            if ms < 1000 then
                ( 1, "ms" )

            else if ms < 60 * 1000 then
                ( 1000, " seconds" )

            else if ms < 60 * 60 * 1000 then
                ( 60 * 1000, " minutes" )

            else if ms < 48 * 60 * 60 * 1000 then
                ( 60 * 60 * 1000, " hours" )

            else if ms < 30 * 24 * 60 * 60 * 1000 then
                ( 24 * 60 * 60 * 1000, " days" )

            else if ms < 365 * 24 * 60 * 60 * 1000 then
                ( 30 * 60 * 60 * 1000, " months" )

            else
                ( 365 * 24 * 60 * 60 * 1000, " years" )
    in
    (if future then
        "in "

     else
        ""
    )
        ++ String.fromInt (ms // scale)
        ++ suffix
        ++ (if future then
                ""

            else
                " ago"
           )


formatTime : Time.Posix -> String
formatTime =
    Iso8601.fromTime


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrame Tick
