module Main exposing (..)

import Browser
import Html
import Html.Attributes
import Html.Events
import Maybe
import String
import Task
import Time
import Timer exposing (timer)
import Scroll exposing (scroll)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { timeAtInit : Int
    , counter : Int
    , inputValue : Maybe Int
    }


type Msg
    = InitialTime Time.Posix
    | Increment
    | SetValue String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { timeAtInit = 0
      , counter = 0
      , inputValue = Just 0
      }
    , Task.perform InitialTime Time.now
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Time.every 1000 Tick


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        InitialTime v ->
            ( { model | timeAtInit = Time.posixToMillis v, inputValue = Just (Time.posixToMillis v) }, Cmd.none )

        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        SetValue v ->
            ( { model | inputValue = String.toInt v }, Cmd.none )



-- RequestTime ->
--     ( model, Task.perform Tick Time.now )



view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.ul []
            [ Html.li []
                [ Html.text "Scroll: "
                , Html.node "elm-scroll"
                    [ Html.Attributes.attribute "phrase" "this is a custom element embedded in another elm application"
                    , Html.Attributes.attribute "speed" "60.0"
                    ]
                    []
                ]
            , Html.li [] [ Html.text "Scroll with custom node: ", scroll "this is a test using a custom node" 20 (Just 60.0) ]
            , Html.li [] [ Html.text "Time since init: ", timer model.timeAtInit ]
            , Html.li [] [ Html.text "Time since epoch: ", timer 0 ]
            , Html.li [] [ Html.text "Time since xmas: ", timer 1703487600000 ]
            , Html.li [] [ Html.text "Time until next xmas: ", timer 1735110000000 ]
            , Html.li []
                [ Html.text "Time since"
                , Html.input
                    [ Html.Events.onInput SetValue
                    , Html.Attributes.value
                        (Maybe.withDefault "" <| Maybe.map String.fromInt model.inputValue)
                    ]
                    []
                , model.inputValue
                    |> Maybe.map timer
                    |> Maybe.withDefault (Html.text "invalid input")
                ]
            ]
        ]
