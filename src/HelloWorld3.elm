module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = ""
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    String



-- UPDATE


type Msg
    = SayHello String
    | SayBye


update : Msg -> Model -> Model
update msg model =
    case msg of
        SayHello greeted ->
            "こんにちは" ++ greeted

        SayBye ->
            "さようなら"



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick (SayHello "世界樹") ] [ text "hello" ]
        , button [ onClick SayBye ] [ text "bye" ]
        , span [ class "blue" ] [ text model ]
        ]
