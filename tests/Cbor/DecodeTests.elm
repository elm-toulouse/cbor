module Cbor.DecodeTests exposing (suite)

{-| Tests for the various decoding primitives available from the library.
Test vectors are provided in Appendix A of the RFC 7049, as well as a couple of
additions that are more elm-specific, obtained from an online tool <http://cbor.me/>
which provides bidirectional conversion from raw bytes to CBOR, and vice-versa.
-}

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as E
import Cbor exposing (CborItem(..))
import Cbor.Decode exposing (..)
import Cbor.Tag exposing (Tag(..))
import Dict
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Cbor.Decode"
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
                |> expect bytes (Just << Tuple.second <| hex [])
            , hex [ 0x41, 0x14 ]
                |> expect bytes (Just << Tuple.second <| hex [ 0x14 ])
            , hex [ 0x43, 0x14, 0x42, 0xFF ]
                |> expect bytes (Just << Tuple.second <| hex [ 0x14, 0x42, 0xFF ])
            , hex [ 0x5F, 0x42, 0x01, 0x02, 0x43, 0x03, 0x04, 0x05, 0xFF ]
                |> expect bytes (Just << Tuple.second <| hex [ 0x01, 0x02, 0x03, 0x04, 0x05 ])
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
            , hex [ 0x7F, 0x61, 0x61, 0x61, 0x62, 0xFF ]
                |> expect string (Just "ab")
            , hex [ 0xB0, 0x00, 0x00, 0x00 ]
                |> expect string Nothing
            ]
        , describe "Major Type 4: an array of data items"
            [ hex [ 0x80 ]
                |> expect (list int) (Just [])
            , hex [ 0x83, 0x0E, 0x18, 0x2A, 0x19, 0x05, 0x39, 0x00, 0x00 ]
                |> expect (list int) (Just [ 14, 42, 1337 ])
            , hex [ 0x83, 0x81, 0x01, 0x82, 0x02, 0x03, 0x82, 0x04, 0x05 ]
                |> expect (list (list int)) (Just [ [ 1 ], [ 2, 3 ], [ 4, 5 ] ])
            , hex ([ 0x98, 0x19 ] ++ List.repeat 25 0x00)
                |> expect (list int) (Just (List.repeat 25 0))
            , hex [ 0x82, 0x0E, 0xFF ]
                |> expect (list int) Nothing
            , hex [ 0x82, 0x0E ]
                |> expect (list int) Nothing
            , hex [ 0x9F, 0x01, 0x02, 0x03, 0x04, 0xFF ]
                |> expect (list int) (Just [ 1, 2, 3, 4 ])
            , hex [ 0x9F, 0x81, 0x01, 0x9F, 0x02, 0x02, 0xFF, 0x82, 0x03, 0x03, 0xFF ]
                |> expect (list (list int)) (Just [ [ 1 ], [ 2, 2 ], [ 3, 3 ] ])
            , hex [ 0x00 ]
                |> expect (int |> map (\x -> x + 1)) (Just 1)
            , hex [ 0x82, 0x00, 0x00 ]
                |> expect (list (int |> map (\x -> x + 1))) (Just [ 1, 1 ])
            , hex [ 0x84, 0x01, 0x61, 0x61, 0x02, 0x61, 0x62 ]
                |> expect (list int) Nothing
            , hex [ 0x80 ]
                |> expect length (Just 0)
            , hex [ 0x82 ]
                |> expect length (Just 2)
            , hex [ 0x98, 0x1A ]
                |> expect length (Just 26)
            , hex [ 0x9F ]
                |> expect length Nothing
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
            , hex [ 0xBF, 0x61, 0x61, 0x0E, 0x61, 0x62, 0x18, 0x2A, 0xFF ]
                |> expect (dict string int)
                    (Just <|
                        Dict.fromList [ ( "a", 14 ), ( "b", 42 ) ]
                    )
            , hex [ 0xBF, 0x61, 0x61, 0x9F, 0xBF, 0x61, 0x62, 0x0E, 0xFF, 0xFF, 0xFF ]
                |> expect (dict string (list (dict string int)))
                    (Just <|
                        Dict.fromList [ ( "a", [ Dict.fromList [ ( "b", 14 ) ] ] ) ]
                    )
            , hex [ 0xA0 ]
                |> expect size (Just 0)
            , hex [ 0xA2 ]
                |> expect size (Just 2)
            , hex [ 0xB8, 0x1A ]
                |> expect size (Just 26)
            , hex [ 0xBF ]
                |> expect size Nothing
            ]
        , describe "Major type 6: tags"
            [ hex
                ([ 0xC0, 0x74, 0x32, 0x30, 0x31, 0x33, 0x2D, 0x30 ]
                    ++ [ 0x33, 0x2D, 0x32, 0x31, 0x54, 0x32, 0x30 ]
                    ++ [ 0x3A, 0x30, 0x34, 0x3A, 0x30, 0x30, 0x5A ]
                )
                |> expect
                    (tagged StandardDateTime string)
                    (Just ( StandardDateTime, "2013-03-21T20:04:00Z" ))
            , hex [ 0xC1, 0x1A, 0x51, 0x4B, 0x67, 0xB0 ]
                |> expect (tagged EpochDateTime int) (Just ( EpochDateTime, 1363896240 ))
            , hex [ 0xD7, 0x44, 0x01, 0x02, 0x03, 0x04 ]
                |> expect (tagged Base16Conversion bytes) (Just ( Base16Conversion, Tuple.second <| hex [ 0x01, 0x02, 0x03, 0x04 ] ))
            , hex [ 0xD8, 0x18, 0x45, 0x64, 0x49, 0x45, 0x54, 0x46 ]
                |> expect (tagged Cbor bytes) (Just ( Cbor, Tuple.second <| hex [ 0x64, 0x49, 0x45, 0x54, 0x46 ] ))
            , hex [ 0xC0, 0x00, 0x00 ]
                |> expect (tagged Cbor string) Nothing
            , hex [ 0xD8, 0x2A, 0x0E ]
                |> expect tag (Just (Unknown 42))
            ]
        , describe "Major type 7: floating-point numbers and simple data types"
            [ hex [ 0xF4 ]
                |> expect bool (Just False)
            , hex [ 0xF5 ]
                |> expect bool (Just True)
            , hex [ 0xFF ]
                |> expect bool Nothing
            , hex [ 0xF9, 0x80, 0x00 ]
                |> expect float (Just -0.0)
            , hex [ 0xF9, 0x3C, 0x00 ]
                |> expect float (Just 1.0)
            , hex [ 0xF9, 0x55, 0x22 ]
                |> expect float (Just 82.125)
            , hex [ 0xF9, 0x04, 0x00 ]
                |> expect float (Just 0.00006103515625)
            , hex [ 0xF9, 0x00, 0x01 ]
                |> expect float (Just 5.960464477539063e-8)
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
            , hex [ 0xF4, 0xFF ]
                |> expect float Nothing
            , hex [ 0xF9, 0xFF, 0xFF ]
                |> expect (map isNaN float) (Just True)
            ]
        , describe "Extras"
            [ hex [ 0xF6 ]
                |> expect (maybe bool) (Just Nothing)
            , hex [ 0xF7 ]
                |> expect (maybe fail) (Just Nothing)
            , hex [ 0x18, 0x50 ]
                |> expect (maybe (map (always 14) fail)) Nothing
            , hex [ 0x18, 0x51 ]
                |> expect (maybe (andThen (always <| succeed 14) fail)) Nothing
            , hex [ 0xF4 ]
                |> expect (maybe bool) (Just (Just False))
            , hex []
                |> expect (succeed 14) (Just 14)
            , hex [ 0x84, 0x00, 0x00, 0x00, 0x00 ]
                |> expect fail Nothing
            , hex []
                |> expect (maybe (succeed 14)) (Just (Just 14))
            , hex [ 0x01 ]
                |> expect (maybe <| maybe int) (Just (Just (Just 1)))
            , hex []
                |> expect (maybe (succeed 1 |> andThen (\n -> succeed (n + 1)))) (Just (Just 2))
            , hex [ 0x00, 0x00 ]
                |> expect (int |> andThen (\_ -> fail) |> andThen succeed) Nothing
            , hex [ 0x83, 0x00, 0x00, 0x00 ]
                |> expect (list (int |> andThen (\_ -> succeed 1))) (Just [ 1, 1, 1 ])
            , hex [ 0x9F, 0x00, 0x00, 0x00, 0x00, 0xFF ]
                |> expect (list (int |> andThen (\_ -> succeed 1))) (Just [ 1, 1, 1, 1 ])
            , hex [ 0x83, 0xF6, 0x01, 0x02 ]
                |> expect (list <| maybe int) (Just [ Nothing, Just 1, Just 2 ])
            , hex [ 0x01 ]
                |> expect
                    (maybe int
                        |> andThen
                            (\m ->
                                case m of
                                    Nothing ->
                                        fail

                                    Just x ->
                                        succeed (x + 1)
                            )
                    )
                    (Just 2)
            , hex [ 0x01, 0x02 ]
                |> expect (map2 Map2 int int) (Just <| Map2 1 2)
            , hex [ 0x01, 0x02, 0x03 ]
                |> expect (map3 Map3 int int int) (Just <| Map3 1 2 3)
            , hex [ 0x01, 0x02, 0x03, 0x04 ]
                |> expect (map4 Map4 int int int int) (Just <| Map4 1 2 3 4)
            , hex [ 0x01, 0x02, 0x03, 0x04, 0x05 ]
                |> expect (map5 Map5 int int int int int) (Just <| Map5 1 2 3 4 5)
            , hex [ 0x5F, 0x41, 0x01, 0x41, 0x02, 0xFF ]
                |> expect
                    (beginBytes
                        |> ignoreThen (map2 Tuple.pair bytes bytes)
                        |> thenIgnore break
                    )
                    (Just ( Tuple.second <| hex [ 0x01 ], Tuple.second <| hex [ 0x02 ] ))
            , hex [ 0x42, 0x41, 0x01, 0x41, 0x02 ]
                |> expect
                    (beginBytes
                        |> ignoreThen (map2 Tuple.pair bytes bytes)
                        |> thenIgnore break
                    )
                    Nothing
            , hex [ 0x5F, 0x41, 0x01, 0x41, 0x02, 0x00 ]
                |> expect
                    (beginBytes
                        |> ignoreThen (map2 Tuple.pair bytes bytes)
                        |> thenIgnore break
                    )
                    Nothing
            , hex [ 0x7F, 0x61, 0x61, 0x61, 0x62, 0xFF ]
                |> expect
                    (beginString
                        |> ignoreThen (map2 Tuple.pair string string)
                        |> thenIgnore break
                    )
                    (Just ( "a", "b" ))
            , hex [ 0x52, 0x61, 0x61, 0x61, 0x62 ]
                |> expect
                    (beginString
                        |> ignoreThen (map2 Tuple.pair string string)
                        |> thenIgnore break
                    )
                    Nothing
            , hex [ 0x9F, 0x01, 0x02, 0xFF ]
                |> expect
                    (beginList
                        |> ignoreThen (map2 Tuple.pair int int)
                        |> thenIgnore break
                    )
                    (Just ( 1, 2 ))
            , hex [ 0x82, 0x01, 0x02 ]
                |> expect
                    (beginList
                        |> ignoreThen (map2 Tuple.pair int int)
                        |> thenIgnore break
                    )
                    Nothing
            , hex [ 0xBF, 0x01, 0x01, 0x02, 0x02, 0xFF ]
                |> expect
                    (beginDict
                        |> ignoreThen (map2 Tuple.pair (map2 Tuple.pair int int) (int |> ignoreThen int))
                        |> thenIgnore break
                    )
                    (Just ( ( 1, 1 ), 2 ))
            , hex [ 0xA2, 0x01, 0x01, 0x02, 0x02 ]
                |> expect
                    (beginDict
                        |> ignoreThen (map2 Tuple.pair (map2 Tuple.pair int int) (int |> ignoreThen int))
                        |> thenIgnore break
                    )
                    Nothing
            , test "traverse empty" <|
                \_ ->
                    decode (traverse (always int) []) (E.encode <| E.sequence [])
                        |> Expect.equal (Just [])
            , hex [ 0xA2, 0x01, 0x01, 0x02, 0x02 ]
                |> expect
                    (let
                        fromCborItem item =
                            case item of
                                CborInt i ->
                                    succeed (CborInt i)

                                CborMap xs ->
                                    map CborMap <|
                                        traverse
                                            (\( a, b ) -> map2 Tuple.pair (fromCborItem a) (fromCborItem b))
                                            xs

                                _ ->
                                    fail
                     in
                     any |> andThen fromCborItem
                    )
                    (Just <| CborMap [ ( CborInt 1, CborInt 1 ), ( CborInt 2, CborInt 2 ) ])
            ]
        , describe "any / raw"
            [ hex [ 0x00 ]
                |> expect any (Just <| CborInt 0)
            , hex [ 0x20 ]
                |> expect any (Just <| CborInt -1)
            , hex [ 0x41, 0x14 ]
                |> expect any (Just << CborBytes << Tuple.second <| hex [ 0x14 ])
            , hex [ 0x64, 0xF0, 0x9F, 0x8C, 0x88 ]
                |> expect any (Just <| CborString "🌈")
            , hex [ 0x82, 0x0E, 0x18, 0x2A ]
                |> expect any (Just <| CborList [ CborInt 14, CborInt 42 ])
            , hex [ 0xA1, 0x66, 0x70, 0x61, 0x74, 0x61, 0x74, 0x65, 0x0E ]
                |> expect any (Just <| CborMap [ ( CborString "patate", CborInt 14 ) ])
            , hex [ 0xD8, 0x2A, 0x0E ]
                |> expect any (Just <| CborTag (Unknown 42) (CborInt 14))
            , hex [ 0x82, 0xF4, 0xF5 ]
                |> expect (list any) (Just <| [ CborBool False, CborBool True ])
            , hex [ 0xF9, 0x55, 0x22 ]
                |> expect any (Just <| CborFloat 82.125)
            , hex [ 0x82, 0x00, 0x01 ]
                |> expect raw (Just <| Tuple.second <| hex [ 0x82, 0x00, 0x01 ])
            ]
        , describe "Record"
            [ hex [ 0xA4, 0x00, 0x0E, 0x01, 0xF5, 0x02, 0x19, 0x05, 0x39, 0x03, 0x00 ]
                |> expect decodeFooCompact (Just <| Foo 14 True (Just 1337) (Just 0))
            , hex [ 0xA3, 0x00, 0x0E, 0x01, 0xF5, 0x03, 0x02 ]
                |> expect decodeFooCompact (Just <| Foo 14 True Nothing (Just 2))
            , hex [ 0xA3, 0x00, 0x0E, 0x01, 0xF5, 0x02, 0x03 ]
                |> expect decodeFooCompact (Just <| Foo 14 True (Just 3) Nothing)
            , hex [ 0xA2, 0x00, 0x00, 0x01, 0xF4 ]
                |> expect decodeFooCompact (Just <| Foo 0 False Nothing Nothing)
            , hex [ 0xBF, 0x00, 0x0E, 0x01, 0xF5, 0x02, 0x19, 0x05, 0x39, 0x03, 0x00, 0xFF ]
                |> expect decodeFooCompact (Just <| Foo 14 True (Just 1337) (Just 0))
            , hex [ 0xBF, 0x00, 0x0E, 0x01, 0xF5, 0x03, 0x02, 0xFF ]
                |> expect decodeFooCompact (Just <| Foo 14 True Nothing (Just 2))
            , hex [ 0xBF, 0x00, 0x0E, 0x01, 0xF5, 0x02, 0x03, 0xFF ]
                |> expect decodeFooCompact (Just <| Foo 14 True (Just 3) Nothing)
            , hex [ 0xBF, 0x00, 0x00, 0x01, 0xF4, 0xFF ]
                |> expect decodeFooCompact (Just <| Foo 0 False Nothing Nothing)
            , hex [ 0xA4, 0x62, 0x61, 0x30, 0x0E, 0x62, 0x61, 0x31, 0xF5, 0x62, 0x61, 0x32, 0x19, 0x05, 0x39, 0x62, 0x61, 0x33, 0x00 ]
                |> expect decodeFooVerbose (Just <| Foo 14 True (Just 1337) (Just 0))
            , hex
                (List.concat
                    [ [ 0xB8, 0x19, 0x00, 0x00, 0x01, 0x00, 0x02, 0x00, 0x03, 0x00, 0x04, 0x00, 0x05 ]
                    , [ 0x00, 0x06, 0x00, 0x07, 0x00, 0x08, 0x00, 0x09, 0x00, 0x0A, 0x00, 0x0B, 0x00 ]
                    , [ 0x0C, 0x00, 0x0D, 0x00, 0x0E, 0x00, 0x0F, 0x00, 0x10, 0x00, 0x11, 0x00, 0x12 ]
                    , [ 0x00, 0x13, 0x00, 0x14, 0x00, 0x15, 0x00, 0x16, 0x00, 0x17, 0x00, 0x18, 0x18 ]
                    , [ 0x00 ]
                    ]
                )
                |> expect decodeManyRecord (Just <| Many 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
            , hex
                (List.concat
                    [ [ 0xBF, 0x00, 0x00, 0x01, 0x00, 0x02, 0x00, 0x03, 0x00, 0x04, 0x00, 0x05, 0x00 ]
                    , [ 0x06, 0x00, 0x07, 0x00, 0x08, 0x00, 0x09, 0x00, 0x0A, 0x00, 0x0B, 0x00, 0x0C ]
                    , [ 0x00, 0x0D, 0x00, 0x0E, 0x00, 0x0F, 0x00, 0x10, 0x00, 0x11, 0x00, 0x12, 0x00 ]
                    , [ 0x13, 0x00, 0x14, 0x00, 0x15, 0x00, 0x16, 0x00, 0x17, 0x00, 0x18, 0x18, 0x00 ]
                    , [ 0xFF ]
                    ]
                )
                |> expect decodeManyRecord (Just <| Many 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
            , hex [ 0xBF, 0x00, 0x00, 0x01, 0xF4 ]
                |> expect decodeFooCompact Nothing
            , hex [ 0xA3, 0x00, 0x0E, 0x03, 0x02, 0x01, 0xF5 ]
                |> expect decodeFooCompact Nothing
            ]
        , describe "Tuples"
            [ hex [ 0x84, 0x0E, 0xF5, 0x19, 0x05, 0x39, 0x00 ]
                |> expect decodeFooTuple (Just <| Foo 14 True (Just 1337) (Just 0))
            , hex [ 0x84, 0x0E, 0xF5, 0x19, 0x05, 0x39, 0x01 ]
                |> expect decodeFooTupleCompact (Just <| Foo 14 True (Just 1337) (Just 1))
            , hex [ 0x83, 0x0E, 0xF5, 0x19, 0x05, 0x39 ]
                |> expect decodeFooTupleCompact (Just <| Foo 14 True (Just 1337) Nothing)
            , hex [ 0x82, 0x0E, 0xF5 ]
                |> expect decodeFooTupleCompact (Just <| Foo 14 True Nothing Nothing)
            , hex [ 0x9F, 0x0E, 0xF5, 0x19, 0x05, 0x39, 0x00, 0xFF ]
                |> expect decodeFooTuple (Just <| Foo 14 True (Just 1337) (Just 0))
            , hex [ 0x9F, 0x0E, 0xF5, 0x19, 0x05, 0x39, 0x01, 0xFF ]
                |> expect decodeFooTupleCompact (Just <| Foo 14 True (Just 1337) (Just 1))
            , hex [ 0x9F, 0x0E, 0xF5, 0x19, 0x05, 0x39, 0xFF ]
                |> expect decodeFooTupleCompact (Just <| Foo 14 True (Just 1337) Nothing)
            , hex [ 0x9F, 0x0E, 0xF5, 0xFF ]
                |> expect decodeFooTupleCompact (Just <| Foo 14 True Nothing Nothing)
            , hex
                (List.concat
                    [ [ 0x98, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
                    , [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
                    , [ 0x00 ]
                    ]
                )
                |> expect decodeManyTuple (Just <| Many 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
            , hex
                (List.concat
                    [ [ 0x9F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
                    , [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
                    , [ 0xFF ]
                    ]
                )
                |> expect decodeManyTuple (Just <| Many 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
            , hex [ 0x9F, 0x0E, 0xF5, 0x19, 0x05, 0x39, 0x00 ]
                |> expect decodeFooTuple Nothing
            , hex [ 0x9F, 0x0E, 0xF5, 0x19, 0x05, 0x39, 0x00, 0xF5 ]
                |> expect decodeFooTuple Nothing
            ]
        ]


{-| Alias / Shortcut to write test cases
-}
expect : Decoder a -> Maybe a -> ( List Int, Bytes ) -> Test
expect decoder output ( readable, input ) =
    test (Debug.toString readable ++ " -> " ++ Debug.toString output) <|
        \_ -> input |> decode decoder |> Expect.equal output


{-| Convert a list of BE unsigned8 to bytes
-}
hex : List Int -> ( List Int, Bytes )
hex xs =
    ( xs, xs |> List.map E.unsignedInt8 >> E.sequence >> E.encode )



{-------------------------------------------------------------------------------
                                  Fixtures
-------------------------------------------------------------------------------}


type alias Map2 =
    { a : Int, b : Int }


type alias Map3 =
    { a : Int, b : Int, c : Int }


type alias Map4 =
    { a : Int, b : Int, c : Int, d : Int }


type alias Map5 =
    { a : Int, b : Int, c : Int, d : Int, e : Int }


type alias Foo =
    { a0 : Int
    , a1 : Bool
    , a2 : Maybe Int
    , a3 : Maybe Int
    }


decodeFooCompact : Decoder Foo
decodeFooCompact =
    record int Foo <|
        fields
            >> field 0 int
            >> field 1 bool
            >> optionalField 2 int
            >> optionalField 3 int


decodeFooVerbose : Decoder Foo
decodeFooVerbose =
    record string Foo <|
        fields
            >> field "a0" int
            >> field "a1" bool
            >> optionalField "a2" int
            >> optionalField "a3" int


decodeFooTupleCompact : Decoder Foo
decodeFooTupleCompact =
    tuple Foo <|
        elems
            >> elem int
            >> elem bool
            >> optionalElem int
            >> optionalElem int


decodeFooTuple : Decoder Foo
decodeFooTuple =
    tuple Foo <|
        elems
            >> elem int
            >> elem bool
            >> elem (maybe int)
            >> elem (maybe int)


{-| Large record for testing field decoders above 23
-}
type alias Many =
    { a0 : Int
    , a1 : Int
    , a2 : Int
    , a3 : Int
    , a4 : Int
    , a5 : Int
    , a6 : Int
    , a7 : Int
    , a8 : Int
    , a9 : Int
    , a10 : Int
    , a11 : Int
    , a12 : Int
    , a13 : Int
    , a14 : Int
    , a15 : Int
    , a16 : Int
    , a17 : Int
    , a18 : Int
    , a19 : Int
    , a20 : Int
    , a21 : Int
    , a22 : Int
    , a23 : Int
    , a24 : Int
    }


decodeManyRecord : Decoder Many
decodeManyRecord =
    record int Many <|
        fields
            >> field 0 int
            >> field 1 int
            >> field 2 int
            >> field 3 int
            >> field 4 int
            >> field 5 int
            >> field 6 int
            >> field 7 int
            >> field 8 int
            >> field 9 int
            >> field 10 int
            >> field 11 int
            >> field 12 int
            >> field 13 int
            >> field 14 int
            >> field 15 int
            >> field 16 int
            >> field 17 int
            >> field 18 int
            >> field 19 int
            >> field 20 int
            >> field 21 int
            >> field 22 int
            >> field 23 int
            >> field 24 int


decodeManyTuple : Decoder Many
decodeManyTuple =
    tuple Many <|
        elems
            >> elem int
            >> elem int
            >> elem int
            >> elem int
            >> elem int
            >> elem int
            >> elem int
            >> elem int
            >> elem int
            >> elem int
            >> elem int
            >> elem int
            >> elem int
            >> elem int
            >> elem int
            >> elem int
            >> elem int
            >> elem int
            >> elem int
            >> elem int
            >> elem int
            >> elem int
            >> elem int
            >> elem int
            >> elem int
