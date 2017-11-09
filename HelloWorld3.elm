module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = { message = "" }
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { message : String }



-- UPDATE


type Msg
    = SayHello String
    | SayBye


update : Msg -> Model -> Model
update msg model =
    case msg of
        SayHello greeted ->
            { model | message = "こんにちは" ++ greeted }

        SayBye ->
            { model | message = "さようなら" }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick (SayHello "世界樹") ] [ text "hello" ]
        , button [ onClick SayBye ] [ text "bye" ]
        , span [ class "blue" ] [ text model.message ]
        ]
