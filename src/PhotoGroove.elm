module PhotoGroove exposing (main)

import Html exposing (div, h1, img, text)
import Html.Attributes exposing (..)


urlPrefix =
    "http://elm-in-action.com/"


view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ] (List.map (\photo -> viewThumpnail model.selectedUrl photo) model.photos)
        ]


viewThumpnail selectedUrl thumb =
    if selectedUrl == thumb.url then
        img
            [ src (urlPrefix ++ thumb.url)
            , class "selected"
            ]
            []

    else
        img [ src (urlPrefix ++ thumb.url) ]
            []


intialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }


main =
    view intialModel
