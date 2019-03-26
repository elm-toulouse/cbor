module CBOR.DecodeTests exposing (suite)

import Bytes exposing (Endianness(..))
import Bytes.Encode as Bytes
import CBOR.Decode exposing (decode, decodeInt, decodeList)
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
        , describe "decodeList"
            [ test "[]" <|
                \_ ->
                    let
                        bytes =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 0x80
                                    ]
                    in
                    Expect.equal (decode (decodeList decodeInt) bytes) (Just [])
            , test "[14,42,1337]" <|
                \_ ->
                    let
                        bytes =
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
                    Expect.equal (decode (decodeList decodeInt) bytes) (Just [ 14, 42, 1337 ])
            , test "[14,??] (not an known element)" <|
                \_ ->
                    let
                        bytes =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 0x82
                                    , Bytes.unsignedInt8 0x0E
                                    , Bytes.unsignedInt8 0x30
                                    ]
                    in
                    Expect.equal (decode (decodeList decodeInt) bytes) Nothing
            , test "[14(,??)] (invalid size)" <|
                \_ ->
                    let
                        bytes =
                            Bytes.encode <|
                                Bytes.sequence
                                    [ Bytes.unsignedInt8 0x82
                                    , Bytes.unsignedInt8 0x0E
                                    ]
                    in
                    Expect.equal (decode (decodeList decodeInt) bytes) Nothing
            ]
        ]
