module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Secrets


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
    = GetAccessToken
    | GotAccessToken (Result Http.Error String)
    | GetTopics
    | GotTopics (Result Http.Error (List Topic))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetAccessToken ->
            ( { model | message = "getting access token..." }, getAccessToken )

        GotAccessToken (Err e) ->
            ( { model | message = toString e }, Cmd.none )

        GotAccessToken (Ok token) ->
            ( { model | accessToken = token, message = "got token" }, Cmd.none )

        GetTopics ->
            ( { model | message = "getting topics..." }, getTopics model.accessToken )

        GotTopics (Err e) ->
            ( { model | message = toString e }, Cmd.none )

        GotTopics (Ok topics) ->
            ( { model | topics = topics, message = "got topics" }, Cmd.none )



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
                    , button [ onClick GetAccessToken ] [ text "ログイン" ]
                    , button [ onClick GetTopics ] [ text "トピックを読み込む" ]
                    ]
                , div [] (List.map topicListItem model.topics)
                ]
            ]
        , div [ id "right-column" ] [ text "..." ]
        ]


topicListItem : Topic -> Html Msg
topicListItem topic =
    div [ class "topic" ] [ text topic.name ]



-- TYPETALK API
-- Getting the access token


getAccessToken : Cmd Msg
getAccessToken =
    let
        url =
            "https://typetalk.com/oauth2/access_token"

        body =
            Http.stringBody "application/x-www-form-urlencoded"
                ("client_id="
                    ++ Secrets.clientId
                    ++ "&client_secret="
                    ++ Secrets.clientSecret
                    ++ "&grant_type=client_credentials"
                    ++ "&scope=my,topic.read,topic.post"
                 -- adjust scope ???
                )

        request =
            Http.post url body decodeAccessToken
    in
    Http.send GotAccessToken request


decodeAccessToken : Decode.Decoder String
decodeAccessToken =
    Decode.field "access_token" Decode.string



-- Getting the list of topics


getTopics : String -> Cmd Msg
getTopics accessToken =
    let
        url =
            "https://typetalk.com/api/v1/topics"
                ++ "?access_token="
                ++ accessToken

        request =
            Http.get url decodeTopics
    in
    Http.send GotTopics request


decodeTopics : Decode.Decoder (List Topic)
decodeTopics =
    Decode.at [ "topics" ] (Decode.list decodeTopic)


decodeTopic : Decode.Decoder Topic
decodeTopic =
    Decode.map2 Topic
        (Decode.at [ "topic", "name" ] Decode.string)
        (Decode.at [ "topic", "id" ] Decode.int)
