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
    ( { message = ""
      , accessToken = ""
      , topics = [ { name = "Topic 1", id = 1 }, { name = "Topic 2", id = 2 } ]
      }
    , Cmd.none
    )



-- MODEL


type alias Model =
    { message : String
    , accessToken : String
    , topics : List Topic
    }


type alias Topic =
    { name : String, id : Int }



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
            ( { model | message = "さようなら" }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ id "left-column" ]
            [ h1 [] [ text "Elmtalk" ]
            , nav []
                [ div [ class "debug" ]
                    [ span [] [ text model.message ]
                    , button [ onClick (SayHello "世界樹") ] [ text "hello" ]
                    , button [ onClick SayBye ] [ text "bye" ]
                    ]
                , div [] (List.map topicListItem model.topics)
                ]
            ]
        , div [ id "right-column" ] [ text "..." ]
        ]


topicListItem : Topic -> Html Msg
topicListItem topic =
    div [ class "topic" ] [ text topic.name ]
