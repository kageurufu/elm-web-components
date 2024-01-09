port module Components exposing (..)

{-
   -  Elements
   -  Web Components from Elm- magically
-}

import Browser
import Html
import Html.Attributes
import Json.Decode


port updateFlags : (String -> msg) -> Sub msg


node name attributes =
    Html.node name attributes []


type MsgWrapper subMsg
    = UpdateFlags String
    | SubMsg subMsg


type alias EncodedFlags =
    Json.Decode.Value


type alias ModelWrapper subModel =
    Result String subModel


type alias Component model msg =
    Program Json.Decode.Value (ModelWrapper model) (MsgWrapper msg)


{-| A hybridized Browser.element, ready for embedding as a web component

The primary differences between Browser.element and Element.component
involve the need to decode and update flags from html attributes at runtime.
Elm doesn't support updating flags, e.g. a re-init, and HTML doesn't support
anything but string or boolean attributes.

`init` still receives the initial flags, already decoded, but in addition a
`attributeUpdate` must be provided, creating a update message with the new
attribute values.

-}
component :
    { init : flags -> ( model, Cmd subMsg )
    , view : model -> Html.Html (MsgWrapper subMsg)
    , update : subMsg -> model -> ( model, Cmd subMsg )
    , subscriptions : model -> Sub subMsg
    , decodeFlags : Json.Decode.Decoder flags
    , attributeUpdate : flags -> subMsg
    }
    -> Component model subMsg
component { init, view, update, subscriptions, decodeFlags, attributeUpdate } =
    Browser.element
        { init = wrapInit { decodeFlags = decodeFlags, init = init }
        , view = wrapView { view = view }
        , update = wrapUpdate { decodeFlags = decodeFlags, attributeUpdate = attributeUpdate, init = init, update = update }
        , subscriptions = wrapSubscriptions { subscriptions = subscriptions }
        }


wrapInit :
    { decodeFlags : Json.Decode.Decoder flagData
    , init : flagData -> ( model, Cmd subMsg )
    }
    -> Json.Decode.Value
    -> ( ModelWrapper model, Cmd (MsgWrapper subMsg) )
wrapInit { decodeFlags, init } encodedFlags =
    let
        result =
            Json.Decode.decodeValue decodeFlags encodedFlags
                |> Result.mapError Json.Decode.errorToString
    in
    case result of
        Ok flags ->
            let
                ( data, cmd ) =
                    init flags
            in
            ( Ok data, Cmd.map SubMsg cmd )

        Err s ->
            ( Err s, Cmd.none )


wrapUpdate :
    { decodeFlags : Json.Decode.Decoder flags
    , attributeUpdate : flags -> subMsg
    , init : flags -> ( model, Cmd subMsg )
    , update : subMsg -> model -> ( model, Cmd subMsg )
    }
    -> MsgWrapper subMsg
    -> Result String model
    -> ( Result String model, Cmd (MsgWrapper subMsg) )
wrapUpdate { decodeFlags, attributeUpdate, init, update } msg model =
    case msg of
        UpdateFlags encodedFlags ->
            let
                flagDecodeResult =
                    Json.Decode.decodeString decodeFlags encodedFlags
                        |> Result.mapError Json.Decode.errorToString
            in
            case flagDecodeResult of
                Ok flagData ->
                    let
                        ( newModel, subMsg ) =
                            case model of
                                Ok modelData ->
                                    update (attributeUpdate flagData) modelData

                                Err _ ->
                                    init flagData
                    in
                    ( Ok newModel, Cmd.map SubMsg subMsg )

                Err s ->
                    ( Err s, Cmd.none )

        SubMsg subMsg ->
            case model of
                Ok data ->
                    let
                        ( newData, cmd ) =
                            update subMsg data
                    in
                    ( Ok newData
                    , Cmd.map SubMsg cmd
                    )

                Err s ->
                    ( Err s, Cmd.none )


wrapView :
    { view : model -> Html.Html msg }
    -> Result String model
    -> Html.Html msg
wrapView { view } result =
    case result of
        Ok model ->
            view model

        Err string ->
            Html.span [ Html.Attributes.style "color" "red" ] [ Html.text string ]


wrapSubscriptions : { subscriptions : model -> Sub msg } -> Result String model -> Sub (MsgWrapper msg)
wrapSubscriptions { subscriptions } model =
    Sub.batch
        [ updateFlags UpdateFlags
        , case model of
            Ok data ->
                Sub.map SubMsg (subscriptions data)

            Err _ ->
                Sub.none
        ]
