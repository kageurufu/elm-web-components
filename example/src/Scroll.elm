module Scroll exposing (main, scroll)

import Browser
import Components
import Html
import Html.Attributes
import Json.Decode
import Json.Decode.Extras
import Time


scroll phrase length speed =
    Components.node "elm-scroll"
        (List.concat
            [ [ Html.Attributes.attribute "phrase" phrase
              , Html.Attributes.attribute "length" (String.fromInt length)
              ]
            , case speed of
                Just v ->
                    [ Html.Attributes.attribute "speed" (String.fromFloat v) ]

                Nothing ->
                    []
            ]
        )


main : Components.Component Model Msg
main =
    Components.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , decodeFlags = decodeFlags
        , attributeUpdate = FlagChange
        }


type alias Flags =
    { phrase : String, length : Int, speed : Float }


type alias Model =
    { phrase : String, length : Int, speed : Float, currentOffset : Int }


type Msg
    = Tick Time.Posix
    | FlagChange Flags


decodeFlags : Json.Decode.Decoder Flags
decodeFlags =
    Json.Decode.map3 Flags
        (Json.Decode.field "phrase" Json.Decode.string)
        (Json.Decode.oneOf [ Json.Decode.field "length" (Json.Decode.oneOf [ Json.Decode.int, Json.Decode.Extras.parseInt ]), Json.Decode.succeed 10 ])
        (Json.Decode.oneOf [ Json.Decode.field "speed" (Json.Decode.oneOf [ Json.Decode.float, Json.Decode.Extras.parseFloat ]), Json.Decode.succeed 1000.0 ])


init : Flags -> ( Model, Cmd Msg )
init { phrase, length, speed } =
    ( { phrase = phrase, length = length, speed = speed, currentOffset = 0 }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions { speed } =
    Time.every speed Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model
                | currentOffset =
                    if model.currentOffset > String.length model.phrase then
                        0

                    else
                        model.currentOffset + 1
              }
            , Cmd.none
            )

        FlagChange { length, phrase, speed } ->
            ( { model | phrase = phrase, length = length, speed = speed }, Cmd.none )


view : Model -> Html.Html msg
view model =
    Html.span []
        [ model.phrase
            |> String.slice model.currentOffset (model.currentOffset + model.length)
            |> String.replace " " (String.fromChar '\u{00A0}')
            |> Html.text
        ]
