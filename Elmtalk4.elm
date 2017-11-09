module Main exposing (..)

import Date
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
            ( { model | message = toString e }, Cmd.none )

        GotAccessToken (Ok token) ->
            { model | accessToken = token, message = "got token" } |> update GetTopics

        GetTopics ->
            ( { model | message = "getting topics..." }, getTopics model.accessToken )

        GotTopics (Err e) ->
            ( { model | message = toString e }, Cmd.none )

        GotTopics (Ok topics) ->
            ( { model | topics = topics, message = "got topics" }, Cmd.none )

        GetPosts topic ->
            ( { model | message = "getting posts..." }, getPosts model.accessToken topic )

        GotPosts (Err e) ->
            ( { model | message = toString e }, Cmd.none )

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
post post =
    div []
        [ div [ class "avatar" ]
            [ img [ src post.imageUrl ] []
            ]
        , div [ class "message-main" ]
            [ div []
                [ span [ class "author-name" ] [ text post.author ]
                , span [ class "message-time" ] [ text (formatDate post.createdAt) ]
                ]
            , div [ class "message-text" ] (newlinesToBr post.message)
            ]
        , hr [ class "clear msgSep" ] []
        ]


formatDate : String -> String
formatDate str =
    let
        date =
            Date.fromString str
    in
    case date of
        Ok date ->
            toString (Date.year date)
                ++ "-"
                ++ toString (Date.month date)
                ++ "-"
                ++ toString (Date.day date)
                ++ " "
                ++ toString (Date.hour date)
                ++ ":"
                ++ toString (Date.minute date)
                ++ ":"
                ++ toString (Date.second date)

        Err error ->
            str


newlinesToBr : String -> List (Html Msg)
newlinesToBr str =
    str
        |> String.lines
        |> List.map (\str -> text str)
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



-- Getting the posts for a topic


getPosts : String -> Topic -> Cmd Msg
getPosts accessToken topic =
    let
        url =
            "https://typetalk.com/api/v1/topics/"
                ++ toString topic.id
                ++ "?access_token="
                ++ accessToken

        request =
            Http.get url decodePosts
    in
    Http.send GotPosts request


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
