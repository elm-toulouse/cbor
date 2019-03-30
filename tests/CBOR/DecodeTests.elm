module CBOR.DecodeTests exposing (suite)

{-| Tests for the various decoding primitives available from the library.
It takes data from an online tool <http://cbor.me/> which provides
bidirectional conversion from raw bytes to CBOR, and vice-versa.
-}

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as Bytes
import CBOR.Decode exposing (Decoder, bytes, decodeBytes, int, list, string)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "CBOR.Decode"
        [ describe "Major Type 0: an unsigned integer"
            [ hex [ 0x0E ]
                |> expect int (Just 14)
            , hex [ 0x18, 0x2A ]
                |> expect int (Just 42)
            , hex [ 0x19, 0x05, 0x39 ]
                |> expect int (Just 1337)
            , hex [ 0x1A, 0x00, 0x02, 0x33, 0x56 ]
                |> expect int (Just 144214)
            , hex [ 0x1B, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00 ]
                |> expect int (Just 4294967296)
            , hex [ 0x1B, 0x00, 0x1F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF ]
                |> expect int (Just 9007199254740991)
            , hex [ 0x1B, 0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
                |> expect int Nothing
            ]
        , describe "Major Type 1: a negative integer"
            [ hex [ 0x2D ]
                |> expect int (Just -14)
            , hex [ 0x38, 0x29 ]
                |> expect int (Just -42)
            , hex [ 0x39, 0x05, 0x38 ]
                |> expect int (Just -1337)
            , hex [ 0x3A, 0x00, 0x02, 0x33, 0x55 ]
                |> expect int (Just -144214)
            , hex [ 0x3A, 0xFF, 0xFF, 0xFF, 0xFF ]
                |> expect int (Just -4294967296)
            , hex [ 0x3B, 0x00, 0x1F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF ]
                |> expect int (Just -9007199254740992)
            , hex [ 0x3B, 0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
                |> expect int Nothing
            ]
        , describe "Major Type 2: a byte string"
            [ hex [ 0x40 ]
                |> expect bytes (Just <| hex [])
            , hex [ 0x41, 0x14 ]
                |> expect bytes (Just <| hex [ 0x14 ])
            , hex [ 0x43, 0x14, 0x42, 0xFF ]
                |> expect bytes (Just <| hex [ 0x14, 0x42, 0xFF ])
            ]
        , describe "Major Type 3: a text string"
            [ hex [ 0x60 ]
                |> expect string (Just "")
            , hex [ 0x66, 0x70, 0x61, 0x74, 0x61, 0x74, 0x65 ]
                |> expect string (Just "patate")
            , hex [ 0x64, 0xF0, 0x9F, 0x8C, 0x88 ]
                |> expect string (Just "🌈")
            , hex [ 0x63, 0x70, 0x61 ]
                |> expect string Nothing
            ]
        , describe "Major Type 4: an array of data items"
            [ hex [ 0x80 ]
                |> expect (list int) (Just [])
            , hex [ 0x83, 0x0E, 0x18, 0x2A, 0x19, 0x05, 0x39, 0x00, 0x00 ]
                |> expect (list int) (Just [ 14, 42, 1337 ])
            , hex [ 0x82, 0x0E, 0xFF ]
                |> expect (list int) Nothing
            , hex [ 0x82, 0x0E ]
                |> expect (list int) Nothing
            ]
        ]


{-| Alias / Shortcut to write test cases
-}
expect : Decoder a -> Maybe a -> Bytes -> Test
expect decoder output input =
    test (Debug.toString input ++ " -> " ++ Debug.toString output) <|
        \_ -> input |> decodeBytes decoder |> Expect.equal output


{-| Convert a list of BE unsigned8 to bytes
-}
hex : List Int -> Bytes
hex =
    List.map Bytes.unsignedInt8 >> Bytes.sequence >> Bytes.encode
