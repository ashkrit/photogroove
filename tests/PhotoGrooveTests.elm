module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Html.Attributes exposing (title)
import Json.Decode exposing (decodeString)
import PhotoGroove exposing (..)
import Test exposing (..)


decoderTest : Test
decoderTest =
    test "Title default to (untitled)" <|
        \_ ->
            """{"url":"fruits.com" , "size":5 }"""
                |> decodeString PhotoGroove.photoDecoder
                |> Expect.equal
                    (Ok { title = "(untitle)", url = "fruits.com", size = 5 })


nicedecoderTest : Test
nicedecoderTest =
    test "Title default to (untitled) using nice syntax" <|
        \_ ->
            """{"url":"fruits.com" , "size":5 }"""
                |> decodeString PhotoGroove.photoDecoder
                |> Result.map .title
                |> Expect.equal
                    (Ok "(untitle)")
