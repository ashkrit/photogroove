module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
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
