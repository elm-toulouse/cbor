module CBOR.DecodeTests exposing (suite)

import Bytes exposing (Endianness(..))
import Bytes.Encode as Bytes
import CBOR.Decode exposing (decode, decodeInt)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "CBOR.Decode"
        [ describe "decodeInt"
            [ test "14" <|
                \_ ->
                    let
                        bytes =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 14
                                    ]
                    in
                    Expect.equal (decode decodeInt bytes) (Just 14)
            , test "42" <|
                \_ ->
                    let
                        bytes =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 24
                                    , Bytes.unsignedInt8 42
                                    ]
                    in
                    Expect.equal (decode decodeInt bytes) (Just 42)
            , test "1337" <|
                \_ ->
                    let
                        bytes =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 25
                                    , Bytes.unsignedInt16 BE 1337
                                    ]
                    in
                    Expect.equal (decode decodeInt bytes) (Just 1337)
            , test "144214" <|
                \_ ->
                    let
                        bytes =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 26
                                    , Bytes.unsignedInt32 BE 144214
                                    ]
                    in
                    Expect.equal (decode decodeInt bytes) (Just 144214)
            , test "2^32" <|
                \_ ->
                    let
                        bytes =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 27
                                    , Bytes.unsignedInt32 BE 1
                                    , Bytes.unsignedInt32 BE 0
                                    ]
                    in
                    Expect.equal (decode decodeInt bytes) (Just 4294967296)
            , test "2^53 - 1" <|
                \_ ->
                    let
                        bytes =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 27
                                    , Bytes.unsignedInt32 BE 0x001FFFFF
                                    , Bytes.unsignedInt32 BE 0xFFFFFFFF
                                    ]
                    in
                    Expect.equal (decode decodeInt bytes) (Just 9007199254740991)
            , test "2^53" <|
                \_ ->
                    let
                        bytes =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 27
                                    , Bytes.unsignedInt32 BE 0x00200000
                                    , Bytes.unsignedInt32 BE 0x00
                                    ]
                    in
                    Expect.equal (decode decodeInt bytes) Nothing
            ]
        ]
