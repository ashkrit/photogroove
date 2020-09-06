module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.Attributes exposing (src)
import Json.Decode exposing (decodeString, decodeValue)
import Json.Encode as Encode
import PhotoGroove exposing (..)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag, text)


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


propjsondecoderTest : Test
propjsondecoderTest =
    fuzz2 string int "Title default to (untitled) using property based test" <|
        \url size ->
            [ ( "url", Encode.string url )
            , ( "size", Encode.int size )
            ]
                |> Encode.object
                |> decodeValue PhotoGroove.photoDecoder
                |> Result.map .title
                |> Expect.equal
                    (Ok "(untitle)")


slidHueSetsHue : Test
slidHueSetsHue =
    fuzz int "SlidHue sets the hue" <|
        \amount ->
            intialModel
                |> update (SlidHue amount)
                |> Tuple.first
                |> .hue
                |> Expect.equal amount


noPhotosNoThumbnails : Test
noPhotosNoThumbnails =
    test "No thumbnails" <|
        \_ ->
            intialModel
                |> PhotoGroove.view
                |> Query.fromHtml
                |> Query.findAll [ tag "img" ]
                |> Query.count (Expect.equal 0)
