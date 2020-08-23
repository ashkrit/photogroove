module PhotoGroove exposing (main)

import Browser
import Html exposing (div, h1, img, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ]
            (List.map (viewThumpnail model.selectedUrl) model.photos)
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


update msg model =
    if msg.description == "ClickedPhoto" then
        { model | selectedUrl = msg.data }

    else
        model


viewThumpnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick { description = "ClickedPhoto", data = thumb.url }
        ]
        []


type alias Photo =
    { url : String }


intialModel : { photos : List Photo, selectedUrl : String }
intialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "3.jpeg"
    }


main =
    Browser.sandbox
        { init = intialModel
        , view = view
        , update = update
        }
