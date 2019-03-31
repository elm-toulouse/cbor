module CBOR.DecodeTests exposing (suite)

{-| Tests for the various decoding primitives available from the library.
Test vectors are provided in Appendix A of the RFC 7049, as well as a couple of
additions that are more elm-specific, obtained from an online tool <http://cbor.me/>
which provides bidirectional conversion from raw bytes to CBOR, and vice-versa.
-}

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as Bytes
import CBOR.Decode
    exposing
        ( Decoder
        , bool
        , bytes
        , decodeBytes
        , dict
        , float
        , int
        , list
        , string
        )
import Dict
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "CBOR.Decode"
        [ describe "Major Type 0: an unsigned integer"
            [ hex [ 0x00 ]
                |> expect int (Just 0)
            , hex [ 0x01 ]
                |> expect int (Just 1)
            , hex [ 0x0E ]
                |> expect int (Just 14)
            , hex [ 0x17 ]
                |> expect int (Just 23)
            , hex [ 0x18, 0x18 ]
                |> expect int (Just 24)
            , hex [ 0x18, 0x19 ]
                |> expect int (Just 25)
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
            [ hex [ 0x20 ]
                |> expect int (Just -1)
            , hex [ 0x2D ]
                |> expect int (Just -14)
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
            , hex [ 0x83, 0x81, 0x01, 0x82, 0x02, 0x03, 0x82, 0x04, 0x05 ]
                |> expect (list (list int)) (Just [ [ 1 ], [ 2, 3 ], [ 4, 5 ] ])
            , hex [ 0x82, 0x0E, 0xFF ]
                |> expect (list int) Nothing
            , hex [ 0x82, 0x0E ]
                |> expect (list int) Nothing
            ]
        , describe "Major Type 5: a map of pairs of data items"
            [ hex [ 0xA0 ]
                |> expect (dict string int) (Just Dict.empty)
            , hex [ 0xA1, 0x0E, 0x18, 0x2A ]
                |> expect (dict int int)
                    (Just <|
                        Dict.fromList [ ( 14, 42 ) ]
                    )
            , hex [ 0xA1, 0x66, 0x70, 0x61, 0x74, 0x61, 0x74, 0x65, 0x0E ]
                |> expect (dict string int)
                    (Just <|
                        Dict.fromList [ ( "patate", 14 ) ]
                    )
            , hex [ 0xA1, 0x0E, 0x66, 0x70, 0x61, 0x74, 0x61, 0x74, 0x65 ]
                |> expect (dict int string)
                    (Just <|
                        Dict.fromList [ ( 14, "patate" ) ]
                    )
            , hex [ 0xA1, 0x61, 0x61, 0x82, 0x0E, 0x18, 0x2A ]
                |> expect (dict string (list int))
                    (Just <|
                        Dict.fromList [ ( "a", [ 14, 42 ] ) ]
                    )
            , hex [ 0xA1 ]
                |> expect (dict string int) Nothing
            ]
        , describe "Major type 7: floating-point numbers and simple data types"
            [ hex [ 0xF4 ]
                |> expect bool (Just False)
            , hex [ 0xF5 ]
                |> expect bool (Just True)
            , hex [ 0xF9, 0x80, 0x00 ]
                |> expect float (Just -0.0)
            , hex [ 0xF9, 0x3C, 0x00 ]
                |> expect float (Just 1.0)
            , hex [ 0xF9, 0x55, 0x22 ]
                |> expect float (Just 82.125)
            , hex [ 0xFB, 0x3F, 0xF1, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9A ]
                |> expect float (Just 1.1)
            , hex [ 0xF9, 0x3E, 0x00 ]
                |> expect float (Just 1.5)
            , hex [ 0xFA, 0x47, 0xC3, 0x50, 0x00 ]
                |> expect float (Just 100000.0)
            , hex [ 0xFB, 0x7E, 0x37, 0xE4, 0x3C, 0x88, 0x00, 0x75, 0x9C ]
                |> expect float (Just 1.0e300)
            , hex [ 0xFB, 0xC0, 0x10, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66 ]
                |> expect float (Just -4.1)
            , hex [ 0xF9, 0x7C, 0x00 ]
                |> expect float (Just (1 / 0))
            , hex [ 0xF9, 0xFC, 0x00 ]
                |> expect float (Just (-1 / 0))
            , hex [ 0xFA, 0x7F, 0x80, 0x00, 0x00 ]
                |> expect float (Just (1 / 0))
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