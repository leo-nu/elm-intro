module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { message = "" }, Cmd.none )



-- MODEL


type alias Model =
    { message : String }



-- UPDATE


type Msg
    = SayHello String
    | SayBye


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SayHello greeted ->
            ( { model | message = "こんにちは" ++ greeted }, Cmd.none )

        SayBye ->
            ( { model | message = "..." }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick (SayHello "世界樹") ] [ text "hello" ]
        , button [ onClick SayBye ] [ text "bye" ]
        , span [ class "blue" ] [ text model.message ]
        ]
