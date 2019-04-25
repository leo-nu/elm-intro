module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Iso8601
import Json.Decode as Decode
import Secrets
import Time


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
      , topics = []
      , posts = []
      }
    , getAccessToken
    )



-- MODEL


type alias Model =
    { message : String
    , accessToken : String
    , topics : List Topic
    , posts : List Post
    }


type alias Topic =
    { name : String, id : Int }


type alias Post =
    { message : String
    , author : String
    , createdAt : String
    , imageUrl : String
    }



-- UPDATE


type Msg
    = GetAccessToken
    | GotAccessToken (Result Http.Error String)
    | GetTopics
    | GotTopics (Result Http.Error (List Topic))
    | GetPosts Topic
    | GotPosts (Result Http.Error (List Post))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetAccessToken ->
            ( { model | message = "getting access token..." }, getAccessToken )

        GotAccessToken (Err e) ->
            ( { model | message = Debug.toString e }, Cmd.none )

        GotAccessToken (Ok token) ->
            { model | accessToken = token, message = "got token" } |> update GetTopics

        GetTopics ->
            ( { model | message = "getting topics..." }, getTopics model.accessToken )

        GotTopics (Err e) ->
            ( { model | message = Debug.toString e }, Cmd.none )

        GotTopics (Ok topics) ->
            ( { model | topics = topics, message = "got topics" }, Cmd.none )

        GetPosts topic ->
            ( { model | message = "getting posts..." }, getPosts model.accessToken topic )

        GotPosts (Err e) ->
            ( { model | message = Debug.toString e }, Cmd.none )

        GotPosts (Ok posts) ->
            ( { model | posts = posts, message = "got posts" }, Cmd.none )



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
        , div [ id "right-column" ]
            [ div [ class "messages" ] (List.map post model.posts)
            ]
        ]


topicListItem : Topic -> Html Msg
topicListItem topic =
    div [ class "topic", onClick (GetPosts topic) ] [ text topic.name ]


post : Post -> Html Msg
post postData =
    div []
        [ div [ class "avatar" ]
            [ img [ src postData.imageUrl ] []
            ]
        , div [ class "message-main" ]
            [ div []
                [ span [ class "author-name" ] [ text postData.author ]
                , span [ class "message-time" ] [ text (formatDate postData.createdAt) ]
                ]
            , div [ class "message-text" ] (newlinesToBr postData.message)
            ]
        , hr [ class "clear msgSep" ] []
        ]


formatDate : String -> String
formatDate str =
    let
        parsedDate =
            Iso8601.toTime str

        jstTimezone =
            Time.customZone (9 * 60) []
    in
    case parsedDate of
        Ok date ->
            String.fromInt (Time.toYear jstTimezone date)
                ++ "-"
                ++ Debug.toString (Time.toMonth jstTimezone date)
                ++ "-"
                ++ String.fromInt (Time.toDay jstTimezone date)
                ++ " "
                ++ String.fromInt (Time.toHour jstTimezone date)
                ++ ":"
                ++ String.fromInt (Time.toMinute jstTimezone date)
                ++ ":"
                ++ String.fromInt (Time.toSecond jstTimezone date)

        Err error ->
            str


newlinesToBr : String -> List (Html Msg)
newlinesToBr str =
    String.lines str
        |> List.map (\s -> text s)
        |> List.intersperse (br [] [])



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



-- Getting the list of topics


getTopics : String -> Cmd Msg
getTopics accessToken =
    let
        url =
            "https://typetalk.com/api/v2/topics"
                ++ "?access_token="
                ++ accessToken
                ++ "&spaceKey="
                ++ Secrets.spaceKey
    in
    Http.get
        { url = url
        , expect = Http.expectJson GotTopics decodeTopics
        }


decodeTopics : Decode.Decoder (List Topic)
decodeTopics =
    Decode.at [ "topics" ] (Decode.list decodeTopic)


decodeTopic : Decode.Decoder Topic
decodeTopic =
    Decode.map2 Topic
        (Decode.at [ "topic", "name" ] Decode.string)
        (Decode.at [ "topic", "id" ] Decode.int)



-- Getting the posts for a topic


getPosts : String -> Topic -> Cmd Msg
getPosts accessToken topic =
    let
        url =
            "https://typetalk.com/api/v1/topics/"
                ++ String.fromInt topic.id
                ++ "?access_token="
                ++ accessToken
    in
    Http.get
        { url = url
        , expect = Http.expectJson GotPosts decodePosts
        }


decodePosts : Decode.Decoder (List Post)
decodePosts =
    Decode.at [ "posts" ] (Decode.list decodePost)


decodePost : Decode.Decoder Post
decodePost =
    Decode.map4 Post
        (Decode.at [ "message" ] Decode.string)
        (Decode.at [ "account", "fullName" ] Decode.string)
        (Decode.at [ "createdAt" ] Decode.string)
        (Decode.at [ "account", "imageUrl" ] Decode.string)
