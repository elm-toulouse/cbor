module CBOR.DecodeTests exposing (suite)

{-| Tests for the various decoding primitives available from the library.
It takes data from an online tool <http://cbor.me/> which provides
bidirectional conversion from raw bytes to CBOR, and vice-versa.
-}

import Bytes exposing (Endianness(..))
import Bytes.Encode as Bytes
import CBOR.Decode exposing (bytes, decodeBytes, int, list)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "CBOR.Decode"
        [ describe "Major Type 0"
            [ test "14" <|
                \_ ->
                    let
                        bs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 14
                                    ]
                    in
                    Expect.equal (decodeBytes int bs) (Just 14)
            , test "42" <|
                \_ ->
                    let
                        bs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 24
                                    , Bytes.unsignedInt8 42
                                    ]
                    in
                    Expect.equal (decodeBytes int bs) (Just 42)
            , test "1337" <|
                \_ ->
                    let
                        bs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 25
                                    , Bytes.unsignedInt16 BE 1337
                                    ]
                    in
                    Expect.equal (decodeBytes int bs) (Just 1337)
            , test "144214" <|
                \_ ->
                    let
                        bs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 26
                                    , Bytes.unsignedInt32 BE 144214
                                    ]
                    in
                    Expect.equal (decodeBytes int bs) (Just 144214)
            , test "2^32" <|
                \_ ->
                    let
                        bs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 27
                                    , Bytes.unsignedInt32 BE 1
                                    , Bytes.unsignedInt32 BE 0
                                    ]
                    in
                    Expect.equal (decodeBytes int bs) (Just 4294967296)
            , test "2^53 - 1" <|
                \_ ->
                    let
                        bs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 27
                                    , Bytes.unsignedInt32 BE 0x001FFFFF
                                    , Bytes.unsignedInt32 BE 0xFFFFFFFF
                                    ]
                    in
                    Expect.equal (decodeBytes int bs) (Just 9007199254740991)
            , test "2^53" <|
                \_ ->
                    let
                        bs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 27
                                    , Bytes.unsignedInt32 BE 0x00200000
                                    , Bytes.unsignedInt32 BE 0x00
                                    ]
                    in
                    Expect.equal (decodeBytes int bs) Nothing
            ]
        , describe "Major Type 1"
            [ test "-14" <|
                \_ ->
                    let
                        bs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 0x2D
                                    ]
                    in
                    Expect.equal (decodeBytes int bs) (Just -14)
            , test "-42" <|
                \_ ->
                    let
                        bs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 0x38
                                    , Bytes.unsignedInt8 0x29
                                    ]
                    in
                    Expect.equal (decodeBytes int bs) (Just -42)
            , test "-1337" <|
                \_ ->
                    let
                        bs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 0x39
                                    , Bytes.unsignedInt16 BE 0x0538
                                    ]
                    in
                    Expect.equal (decodeBytes int bs) (Just -1337)
            , test "-144214" <|
                \_ ->
                    let
                        bs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 0x3A
                                    , Bytes.unsignedInt32 BE 0x00023355
                                    ]
                    in
                    Expect.equal (decodeBytes int bs) (Just -144214)
            , test "-2^32" <|
                \_ ->
                    let
                        bs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 0x3A
                                    , Bytes.unsignedInt32 BE 0xFFFFFFFF
                                    ]
                    in
                    Expect.equal (decodeBytes int bs) (Just -4294967296)
            , test "-(2^53 - 1)" <|
                \_ ->
                    let
                        bs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 0x3B
                                    , Bytes.unsignedInt32 BE 0x001FFFFF
                                    , Bytes.unsignedInt32 BE 0xFFFFFFFE
                                    ]
                    in
                    Expect.equal (decodeBytes int bs) (Just -9007199254740991)
            , test "-2^53" <|
                \_ ->
                    let
                        bs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 27
                                    , Bytes.unsignedInt32 BE 0x00200000
                                    , Bytes.unsignedInt32 BE 0x00
                                    ]
                    in
                    Expect.equal (decodeBytes int bs) Nothing
            ]
        , describe "Major Type 2"
            [ test "h''" <|
                \_ ->
                    let
                        bs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 0x40
                                    ]

                        innerbs =
                            Bytes.encode <|
                                Bytes.sequence []
                    in
                    Expect.equal (decodeBytes bytes bs) (Just innerbs)
            , test "h'14'" <|
                \_ ->
                    let
                        bs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 0x41
                                    , Bytes.unsignedInt8 0x14
                                    ]

                        innerbs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 0x14 ]
                    in
                    Expect.equal (decodeBytes bytes bs) (Just innerbs)
            , test "h'1442FF'" <|
                \_ ->
                    let
                        bs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 0x43
                                    , Bytes.unsignedInt8 0x14
                                    , Bytes.unsignedInt8 0x42
                                    , Bytes.unsignedInt8 0xFF
                                    ]

                        innerbs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 0x14
                                    , Bytes.unsignedInt8 0x42
                                    , Bytes.unsignedInt8 0xFF
                                    ]
                    in
                    Expect.equal (decodeBytes bytes bs) (Just innerbs)
            ]
        , describe "Major Type 4"
            [ test "[]" <|
                \_ ->
                    let
                        bs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 0x80
                                    ]
                    in
                    Expect.equal (decodeBytes (list int) bs) (Just [])
            , test "[14,42,1337]" <|
                \_ ->
                    let
                        bs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 0x83
                                    , Bytes.unsignedInt8 0x0E
                                    , Bytes.unsignedInt8 0x18
                                    , Bytes.unsignedInt8 0x2A
                                    , Bytes.unsignedInt8 0x19
                                    , Bytes.unsignedInt32 BE 0x05390000
                                    ]
                    in
                    Expect.equal (decodeBytes (list int) bs) (Just [ 14, 42, 1337 ])
            , test "[14,??] (not an known element)" <|
                \_ ->
                    let
                        bs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 0x82
                                    , Bytes.unsignedInt8 0x0E
                                    , Bytes.unsignedInt8 0xFF
                                    ]
                    in
                    Expect.equal (decodeBytes (list int) bs) Nothing
            , test "[14(,??)] (invalid size)" <|
                \_ ->
                    let
                        bs =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 0x82
                                    , Bytes.unsignedInt8 0x0E
                                    ]
                    in
                    Expect.equal (decodeBytes (list int) bs) Nothing
            ]
        ]
