module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Secrets


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetAccessToken ->
            ( { model | message = "getting access token..." }, getAccessToken )

        GotAccessToken (Err e) ->
            ( { model | message = Debug.toString e }, Cmd.none )

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
    in
    Http.post
        { url = url
        , body = body
        , expect = Http.expectJson GotAccessToken accessTokenDecoder
        }


accessTokenDecoder : Decode.Decoder String
accessTokenDecoder =
    Decode.field "access_token" Decode.string
