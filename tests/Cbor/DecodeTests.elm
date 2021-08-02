module Cbor.DecodeTests exposing (suite)

{-| Tests for the various decoding primitives available from the library.
Test vectors are provided in Appendix A of the RFC 7049, as well as a couple of
additions that are more elm-specific, obtained from an online tool <http://cbor.me/>
which provides bidirectional conversion from raw bytes to CBOR, and vice-versa.
-}

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as E
import Cbor.Decode
    exposing
        ( Decoder
        , andThen
        , array
        , bool
        , bytes
        , decode
        , dict
        , fail
        , float
        , int
        , list
        , map
        , map2
        , map3
        , map4
        , map5
        , maybe
        , pair
        , record
        , string
        , succeed
        , tag
        , tagged
        )
import Cbor.Tag exposing (Tag(..))
import Dict
import Expect
import Hex.Convert exposing (toBytes)
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
            , hex [ 0x0E, 0x62, 0x34, 0x32 ]
                |> expect (pair int string) (Just ( 14, "42" ))
            , hex [ 0x0E, 0x62, 0x34, 0x32 ]
                |> expect (pair int int) Nothing
            , hex [ 0x83, 0x0E, 0xF5 ]
                |> expect (array <| map2 Tuple.pair int bool) (Just ( 14, True ))
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
            , hex [ 0xF4 ]
                |> expect (maybe bool) (Just (Just False))
            , hex []
                |> expect (succeed 14) (Just 14)
            , hex []
                |> expect (maybe (succeed 14)) (Just (Just 14))
            , hex [ 0x01 ]
                |> expect (maybe <| maybe int) (Just (Just (Just 1)))
            , hex [ 0x00 ]
                |> expect (int |> map (\x -> x + 1)) (Just 1)
            , hex []
                |> expect (maybe (succeed 1 |> andThen (\n -> succeed (n + 1)))) (Just (Just 2))
            , hex [ 0x84, 0x00, 0x00, 0x00, 0x00 ]
                |> expect fail Nothing
            , hex [ 0xB0, 0x00, 0x00, 0x00 ]
                |> expect string Nothing
            , hex [ 0x00, 0x00 ]
                |> expect (int |> andThen (\_ -> fail) |> andThen succeed) Nothing
            , hex [ 0x82, 0x00, 0x00 ]
                |> expect (list (int |> map (\x -> x + 1))) (Just [ 1, 1 ])
            , hex [ 0x83, 0x00, 0x00, 0x00 ]
                |> expect (list (int |> andThen (\_ -> succeed 1))) (Just [ 1, 1, 1 ])
            , hex [ 0x9F, 0x00, 0x00, 0x00, 0x00, 0xFF ]
                |> expect (list (int |> andThen (\_ -> succeed 1))) (Just [ 1, 1, 1, 1 ])
            , hex [ 0x9F, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF ]
                |> expect (list (succeed 1)) (Just [ 1, 1, 1, 1, 1 ])
            , hex [ 0x84, 0x01, 0x61, 0x61, 0x02, 0x61, 0x62 ]
                |> expect (list int) Nothing
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
            ]
        , describe "golden"
            [ "D2844DA20126044870AAA4460B56D17CA0590101A401624652041A611053E0061A60BFE860390103A101A4617681AA626369781D75726E3A757663693A30313A46523A4B4B46334259494759535646235162636F62465262646E016264746A323032312D30332D303162697364434E414D626D616D4F52472D313030303330323135626D706C45552F312F32302F3135323862736402627467693834303533393030366276706A3131313933343930303763646F626A313936322D30352D3331636E616DA462666E6F7468656F756C6520737572206D657262676E6B6A65616E2070696572726563666E746F5448454F554C453C5355523C4D455263676E746B4A45414E3C5049455252456376657265312E302E305840EB4B5342A37817B5D0C6DA80AAF17D364EA080ADA2369658666E2CB8C64BEB6537FE9EA082DAA7081F16ACFCE3339CCAF31CF06711FB0AC4D1811E481AABDC3F"
                |> expectStr
                    (tagged (Unknown 18) <|
                        array <|
                            map4 CoseEnvelope
                                bytes
                                (record <| succeed ())
                                bytes
                                bytes
                    )
            ]
        ]


type alias Map2 =
    { a : Int, b : Int }


type alias Map3 =
    { a : Int, b : Int, c : Int }


type alias Map4 =
    { a : Int, b : Int, c : Int, d : Int }


type alias Map5 =
    { a : Int, b : Int, c : Int, d : Int, e : Int }


type alias CoseEnvelope =
    { protected : Bytes
    , unprotected : ()
    , payload : Bytes
    , signature : Bytes
    }


{-| Alias / Shortcut to write test cases
-}
expect : Decoder a -> Maybe a -> ( List Int, Bytes ) -> Test
expect decoder output ( readable, input ) =
    test (Debug.toString readable ++ " -> " ++ Debug.toString output) <|
        \_ -> input |> decode decoder |> Expect.equal output


{-| Like 'expect' but works from a hex-encoded input string
-}
expectStr : Decoder a -> String -> Test
expectStr decoder input =
    test input <|
        \_ ->
            case toBytes input of
                Nothing ->
                    Expect.fail "couldn't decode base16 input"

                Just raw ->
                    case decode decoder raw of
                        Nothing ->
                            Expect.fail "couldn't decode CBOR"

                        Just _ ->
                            Expect.pass


{-| Convert a list of BE unsigned8 to bytes
-}
hex : List Int -> ( List Int, Bytes )
hex xs =
    ( xs, xs |> List.map E.unsignedInt8 >> E.sequence >> E.encode )
