module Cbor.EncodeTests exposing (suite)

{-| Tests for the various encoding primitives available from the library.
Test vectors are provided in Appendix A of the RFC 7049, as well as a couple of
additions that are more elm-specific, obtained from an online tool <http://cbor.me/>
which provides bidirectional conversion from raw bytes to CBOR, and vice-versa.
-}

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as Bytes
import Cbor.Encode
    exposing
        ( Encoder
        , bool
        , encode
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
            , int 9007199254740991
                |> expect [ 0x1B, 0x00, 0x1F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF ]
            ]
        , describe "Major type 7: floating-point numbers and simple data types"
            [ bool True
                |> expect [ 0xF5 ]
            , bool False
                |> expect [ 0xF4 ]
            ]
        ]


{-| Alias / Shortcut to write test cases
-}
expect : List Int -> Encoder -> Test
expect output input =
    test (Debug.toString input ++ " -> " ++ Debug.toString output) <|
        \_ -> encode input |> Expect.equal (hex output)


{-| Convert a list of BE unsigned8 to bytes
-}
hex : List Int -> Bytes
hex =
    List.map Bytes.unsignedInt8 >> Bytes.sequence >> Bytes.encode
