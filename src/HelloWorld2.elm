module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)


main =
    Html.div [] [ Html.text "こんにちは世界樹" ]



-- Step 2


main2 =
    Html.div []
        [ Html.span [] [ Html.text "こんにちは" ]
        , Html.span [ Html.Attributes.class "red" ] [ text "世界樹" ]
        ]


main3 =
    div []
        [ span [] [ text "こんにちは" ]
        , span [ class "red" ] [ text "世界樹" ]
        ]
