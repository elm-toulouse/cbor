module Cbor.EncodeTests exposing (suite)

{-| Tests for the various encoding primitives available from the library.
Test vectors are provided in Appendix A of the RFC 7049, as well as a couple of
additions that are more elm-specific, obtained from an online tool <http://cbor.me/>
which provides bidirectional conversion from raw bytes to CBOR, and vice-versa.
-}

import Bytes exposing (Bytes, width)
import Bytes.Decode as Bytes
import Cbor.Encode
    exposing
        ( Encoder
        , bool
        , encode
        , float
        , float16
        , float32
        , int
        )
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
        , describe "Major type 7: floating-point numbers and simple data types"
            [ bool True
                |> expect [ 0xF5 ]
            , bool False
                |> expect [ 0xF4 ]
            , float -0.0
                |> expect [ 0xFB, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
            , float 1.1
                |> expect [ 0xFB, 0x3F, 0xF1, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9A ]
            , float 1.0e300
                |> expect [ 0xFB, 0x7E, 0x37, 0xE4, 0x3C, 0x88, 0x00, 0x75, 0x9C ]
            , float -4.1
                |> expect [ 0xFB, 0xC0, 0x10, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66 ]
            , float32 0.0
                |> expect [ 0xFA, 0x00, 0x00, 0x00, 0x00 ]
            , float32 100000.0
                |> expect [ 0xFA, 0x47, 0xC3, 0x50, 0x00 ]
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
        |> Bytes.decode
            (Bytes.loop ( width bytes, [] )
                (\( n, xs ) ->
                    if n == 0 then
                        xs |> List.reverse |> Bytes.Done |> Bytes.succeed

                    else
                        Bytes.unsignedInt8
                            |> Bytes.map (\x -> Bytes.Loop ( n - 1, x :: xs ))
                )
            )
