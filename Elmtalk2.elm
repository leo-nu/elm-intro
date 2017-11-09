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
    = SayHello String
    | GetAccessToken
    | GotAccessToken (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SayHello greeted ->
            ( { model | message = "こんにちは" ++ greeted }, Cmd.none )

        GetAccessToken ->
            ( { model | message = "getting access token..." }, getAccessToken )

        GotAccessToken (Err e) ->
            ( { model | message = toString e }, Cmd.none )

        GotAccessToken (Ok token) ->
            ( { model | accessToken = token, message = "got token" }, Cmd.none )



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
                    , button [ onClick GetAccessToken ] [ text "ログイン" ]
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
                )

        request =
            Http.post url body decodeAccessToken
    in
    Http.send GotAccessToken request


decodeAccessToken : Decode.Decoder String
decodeAccessToken =
    Decode.field "access_token" Decode.string
