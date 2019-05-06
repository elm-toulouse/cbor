module Cbor.TagTests exposing (suite)

import Cbor.Decode as D
import Cbor.Encode as E
import Cbor.Tag exposing (Tag(..))
import Expect
import Fuzz exposing (Fuzzer, constant, oneOf)
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "decode << encode: tags"
        [ fuzz (oneOf tags) "tags" <|
            \t ->
                D.decode D.tag (E.encode (E.tag t)) |> Expect.equal (Just t)
        ]


tags : List (Fuzzer Tag)
tags =
    [ constant StandardDateTime
    , constant EpochDateTime
    , constant PositiveBigNum
    , constant NegativeBigNum
    , constant DecimalFraction
    , constant BigFloat
    , constant Base64UrlConversion
    , constant Base64Conversion
    , constant Base16Conversion
    , constant Cbor
    , constant Uri
    , constant Base64Url
    , constant Base64
    , constant Regex
    , constant Mime
    , constant IsCbor
    , constant <| Unknown 42
    , constant <| Unknown 58737
    , constant <| Unknown 3287451
    ]
