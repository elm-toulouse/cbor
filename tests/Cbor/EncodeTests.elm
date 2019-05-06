module Cbor.EncodeTests exposing (suite)

{-| Tests for the various encoding primitives available from the library.
Test vectors are provided in Appendix A of the RFC 7049, as well as a couple of
additions that are more elm-specific, obtained from an online tool <http://cbor.me/>
which provides bidirectional conversion from raw bytes to CBOR, and vice-versa.
-}

import Bytes exposing (Bytes, width)
import Bytes.Decode as D
import Bytes.Encode as E
import Cbor.Encode
    exposing
        ( Encoder
        , beginBytes
        , beginDict
        , beginList
        , beginStrings
        , bool
        , break
        , bytes
        , dict
        , encode
        , float
        , float16
        , float32
        , float64
        , int
        , list
        , null
        , pair
        , sequence
        , string
        , tagged
        )
import Cbor.Tag exposing (Tag(..))
import Dict
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Cbor.Encode"
        [ describe "Major Type 0: an unsigned integer"
            [ int 0
                |> expect [ 0x00 ]
            , int 1
                |> expect [ 0x01 ]
            , int 14
                |> expect [ 0x0E ]
            , int 23
                |> expect [ 0x17 ]
            , int 24
                |> expect [ 0x18, 0x18 ]
            , int 25
                |> expect [ 0x18, 0x19 ]
            , int 1337
                |> expect [ 0x19, 0x05, 0x39 ]
            , int 144214
                |> expect [ 0x1A, 0x00, 0x02, 0x33, 0x56 ]
            , int 4294967296
                |> expect [ 0x1B, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00 ]
            , int 10000000000
                |> expect [ 0x1B, 0x00, 0x00, 0x00, 0x02, 0x54, 0x0B, 0xE4, 0x00 ]
            , int 9007199254740991
                |> expect [ 0x1B, 0x00, 0x1F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF ]
            ]
        , describe "Major Type 1: a negative integer"
            [ int -2
                |> expect [ 0x21 ]
            , int -14
                |> expect [ 0x2D ]
            , int -1337
                |> expect [ 0x39, 0x05, 0x38 ]
            , int -144214
                |> expect [ 0x3A, 0x00, 0x02, 0x33, 0x55 ]
            , int -4294967296
                |> expect [ 0x3A, 0xFF, 0xFF, 0xFF, 0xFF ]
            , int -10000000000
                |> expect [ 0x3B, 0x00, 0x00, 0x00, 0x02, 0x54, 0x0B, 0xE3, 0xFF ]
            , int -9007199254740992
                |> expect [ 0x3B, 0x00, 0x1F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF ]
            ]
        , describe "Major Type 2: a byte string"
            [ bytes (toBytes [])
                |> expect [ 0x40 ]
            , bytes (toBytes [ 0x01, 0x02, 0x03, 0x04 ])
                |> expect [ 0x44, 0x01, 0x02, 0x03, 0x04 ]
            , sequence
                [ beginBytes
                , bytes (toBytes [ 0x01, 0x02 ])
                , bytes (toBytes [ 0x03, 0x04, 0x05 ])
                , break
                ]
                |> expect [ 0x5F, 0x42, 0x01, 0x02, 0x43, 0x03, 0x04, 0x05, 0xFF ]
            ]
        , describe "Major Type 3: a text string"
            [ string ""
                |> expect [ 0x60 ]
            , string "a"
                |> expect [ 0x61, 0x61 ]
            , string "IETF"
                |> expect [ 0x64, 0x49, 0x45, 0x54, 0x46 ]
            , string "\"\\"
                |> expect [ 0x62, 0x22, 0x5C ]
            , string "ü"
                |> expect [ 0x62, 0xC3, 0xBC ]
            , string "水"
                |> expect [ 0x63, 0xE6, 0xB0, 0xB4 ]
            , string "🌈"
                |> expect [ 0x64, 0xF0, 0x9F, 0x8C, 0x88 ]
            , sequence [ beginStrings, string "strea", string "ming", break ]
                |> expect
                    ([ 0x7F, 0x65, 0x73, 0x74, 0x72, 0x65, 0x61 ]
                        ++ [ 0x64, 0x6D, 0x69, 0x6E, 0x67, 0xFF ]
                    )
            , sequence [ beginStrings, string "a", string "b", break ]
                |> expect [ 0x7F, 0x61, 0x61, 0x61, 0x62, 0xFF ]
            ]
        , describe "Major Type 4: an array of data-items"
            [ list int []
                |> expect [ 0x80 ]
            , list int [ 1, 2, 3 ]
                |> expect [ 0x83, 0x01, 0x02, 0x03 ]
            , list (list int) [ [ 1 ], [ 2, 3 ], [ 4, 5 ] ]
                |> expect [ 0x83, 0x81, 0x01, 0x82, 0x02, 0x03, 0x82, 0x04, 0x05 ]
            , list int [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25 ]
                |> expect
                    ([ 0x98, 0x19, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07 ]
                        ++ [ 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10 ]
                        ++ [ 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x18 ]
                        ++ [ 0x18, 0x19 ]
                    )
            , sequence [ beginList, break ]
                |> expect [ 0x9F, 0xFF ]
            , sequence [ beginList, int 1, list int [ 2, 3 ], sequence [ beginList, int 4, int 5, break ], break ]
                |> expect [ 0x9F, 0x01, 0x82, 0x02, 0x03, 0x9F, 0x04, 0x05, 0xFF, 0xFF ]
            , sequence [ beginList, int 1, list int [ 2, 3 ], list int [ 4, 5 ], break ]
                |> expect [ 0x9F, 0x01, 0x82, 0x02, 0x03, 0x82, 0x04, 0x05, 0xFF ]
            , list (\x -> x) [ int 1, list int [ 2, 3 ], sequence [ beginList, int 4, int 5, break ] ]
                |> expect [ 0x83, 0x01, 0x82, 0x02, 0x03, 0x9F, 0x04, 0x05, 0xFF ]
            , list (\x -> x) [ int 1, sequence [ beginList, int 2, int 3, break ], list int [ 4, 5 ] ]
                |> expect [ 0x83, 0x01, 0x9F, 0x02, 0x03, 0xFF, 0x82, 0x04, 0x05 ]
            , sequence ([ beginList ] ++ List.map int [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25 ] ++ [ break ])
                |> expect
                    ([ 0x9F, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07 ]
                        ++ [ 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10 ]
                        ++ [ 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x18 ]
                        ++ [ 0x18, 0x19, 0xFF ]
                    )
            ]
        , describe "Major Type 5: a map of pairs of data-items"
            [ dict int int (Dict.fromList [])
                |> expect [ 0xA0 ]
            , dict int int (Dict.fromList [ ( 1, 2 ), ( 3, 4 ) ])
                |> expect [ 0xA2, 0x01, 0x02, 0x03, 0x04 ]
            , dict string int (Dict.fromList [ ( "a", 1 ), ( "b", 2 ) ])
                |> expect [ 0xA2, 0x61, 0x61, 0x01, 0x61, 0x62, 0x02 ]
            , dict string
                identity
                (Dict.fromList
                    [ ( "a", int 1 )
                    , ( "b", list int [ 2, 3 ] )
                    ]
                )
                |> expect [ 0xA2, 0x61, 0x61, 0x01, 0x61, 0x62, 0x82, 0x02, 0x03 ]
            , dict string
                string
                (Dict.fromList
                    [ ( "a", "A" )
                    , ( "b", "B" )
                    , ( "c", "C" )
                    , ( "d", "D" )
                    , ( "e", "E" )
                    ]
                )
                |> expect
                    ([ 0xA5, 0x61, 0x61, 0x61, 0x41, 0x61, 0x62, 0x61 ]
                        ++ [ 0x42, 0x61, 0x63, 0x61, 0x43, 0x61, 0x64, 0x61 ]
                        ++ [ 0x44, 0x61, 0x65, 0x61, 0x45 ]
                    )
            , sequence
                [ beginDict
                , pair string int ( "a", 1 )
                , pair string int ( "b", 2 )
                , break
                ]
                |> expect
                    [ 0xBF, 0x61, 0x61, 0x01, 0x61, 0x62, 0x02, 0xFF ]
            ]
        , describe "Major Type 6: tags"
            [ tagged StandardDateTime string "2013-03-21T20:04:00Z"
                |> expect
                    ([ 0xC0, 0x74, 0x32, 0x30, 0x31, 0x33, 0x2D, 0x30 ]
                        ++ [ 0x33, 0x2D, 0x32, 0x31, 0x54, 0x32, 0x30 ]
                        ++ [ 0x3A, 0x30, 0x34, 0x3A, 0x30, 0x30, 0x5A ]
                    )
            , tagged EpochDateTime int 1363896240
                |> expect [ 0xC1, 0x1A, 0x51, 0x4B, 0x67, 0xB0 ]
            , tagged EpochDateTime float 1363896240.5
                |> expect [ 0xC1, 0xFB, 0x41, 0xD4, 0x52, 0xD9, 0xEC, 0x20, 0x00, 0x00 ]
            , tagged Base16Conversion bytes (toBytes [ 0x01, 0x02, 0x03, 0x04 ])
                |> expect [ 0xD7, 0x44, 0x01, 0x02, 0x03, 0x04 ]
            , tagged Cbor bytes (encode <| string "IETF")
                |> expect [ 0xD8, 0x18, 0x45, 0x64, 0x49, 0x45, 0x54, 0x46 ]
            ]
        , describe "Major Type 7: floating-point numbers and simple data types"
            [ bool True
                |> expect [ 0xF5 ]
            , bool False
                |> expect [ 0xF4 ]
            , null
                |> expect [ 0xF6 ]
            , float16 0.0
                |> expect [ 0xF9, 0x00, 0x00 ]
            , float16 -0.0
                |> expect [ 0xF9, 0x80, 0x00 ]
            , float16 1.0
                |> expect [ 0xF9, 0x3C, 0x00 ]
            , float16 1.5
                |> expect [ 0xF9, 0x3E, 0x00 ]
            , float16 65504.0
                |> expect [ 0xF9, 0x7B, 0xFF ]
            , float16 5.960464477539063e-8
                |> expect [ 0xF9, 0x00, 0x01 ]
            , float16 0.00006103515625
                |> expect [ 0xF9, 0x04, 0x00 ]
            , float16 -4.0
                |> expect [ 0xF9, 0xC4, 0x00 ]
            , float16 (1 / 0)
                |> expect [ 0xF9, 0x7C, 0x00 ]
            , float16 (-1 / 0)
                |> expect [ 0xF9, 0xFC, 0x00 ]
            , float32 0.0
                |> expect [ 0xFA, 0x00, 0x00, 0x00, 0x00 ]
            , float32 100000.0
                |> expect [ 0xFA, 0x47, 0xC3, 0x50, 0x00 ]
            , float32 3.4028234663852886e38
                |> expect [ 0xFA, 0x7F, 0x7F, 0xFF, 0xFF ]
            , float32 (1 / 0)
                |> expect [ 0xFA, 0x7F, 0x80, 0x00, 0x00 ]
            , float32 (-1 / 0)
                |> expect [ 0xFA, 0xFF, 0x80, 0x00, 0x00 ]
            , float 1.1
                |> expect [ 0xFB, 0x3F, 0xF1, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9A ]
            , float64 1.0e300
                |> expect [ 0xFB, 0x7E, 0x37, 0xE4, 0x3C, 0x88, 0x00, 0x75, 0x9C ]
            , float64 -4.1
                |> expect [ 0xFB, 0xC0, 0x10, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66 ]
            ]
        ]


{-| Alias / Shortcut to write test cases
-}
expect : List Int -> Encoder -> Test
expect output input =
    test (Debug.toString input ++ " -> " ++ Debug.toString output) <|
        \_ -> hex (encode input) |> Expect.equal (Just output)


{-| Convert a list of BE unsigned8 to bytes
-}
hex : Bytes -> Maybe (List Int)
hex bytes =
    bytes
        |> D.decode
            (D.loop ( width bytes, [] )
                (\( n, xs ) ->
                    if n == 0 then
                        xs |> List.reverse |> D.Done |> D.succeed

                    else
                        D.unsignedInt8
                            |> D.map (\x -> D.Loop ( n - 1, x :: xs ))
                )
            )


{-| Convert a list of BE unsigned8 to bytes
-}
toBytes : List Int -> Bytes
toBytes =
    List.map E.unsignedInt8 >> E.sequence >> E.encode
