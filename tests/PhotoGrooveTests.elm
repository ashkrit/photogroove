module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Json.Decode as Decode exposing (decodeString,decodeValue)
import Json.Encode as Encode
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


dynamicjsondecoderTest : Test
dynamicjsondecoderTest =
    test "Title default to (untitled) using dynamic json" <|
        \_ ->
            [ ( "url", Encode.string "fruits.com" )
            , ( "size", Encode.int 5 )
            ]
                |> Encode.object
                |> decodeValue PhotoGroove.photoDecoder
                |> Result.map .title
                |> Expect.equal
                    (Ok "(untitle)")
