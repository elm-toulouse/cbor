module Fixture exposing (..)

import Cbor.Decode exposing (..)


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


decodeFooVerboseFold : Decoder Foo
decodeFooVerboseFold =
    let
        stepDecoder k =
            case k of
                "a0" ->
                    int |> map (\a0 state -> { state | a0 = a0 })

                "a1" ->
                    bool |> map (\a1 state -> { state | a1 = a1 })

                "a2" ->
                    int |> map (\a2 state -> { state | a2 = Just a2 })

                "a3" ->
                    int |> map (\a3 state -> { state | a3 = Just a3 })

                _ ->
                    fail
    in
    fold string stepDecoder { a0 = 0, a1 = False, a2 = Nothing, a3 = Nothing }


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


decodeManyRecordFold : Decoder Many
decodeManyRecordFold =
    fold int
        (\k ->
            case k of
                0 ->
                    int |> map (\a0 st -> { st | a0 = a0 })

                1 ->
                    int |> map (\a1 st -> { st | a1 = a1 })

                2 ->
                    int |> map (\a2 st -> { st | a2 = a2 })

                3 ->
                    int |> map (\a3 st -> { st | a3 = a3 })

                4 ->
                    int |> map (\a4 st -> { st | a4 = a4 })

                5 ->
                    int |> map (\a5 st -> { st | a5 = a5 })

                6 ->
                    int |> map (\a6 st -> { st | a6 = a6 })

                7 ->
                    int |> map (\a7 st -> { st | a7 = a7 })

                8 ->
                    int |> map (\a8 st -> { st | a8 = a8 })

                9 ->
                    int |> map (\a9 st -> { st | a9 = a9 })

                10 ->
                    int |> map (\a10 st -> { st | a10 = a10 })

                11 ->
                    int |> map (\a11 st -> { st | a11 = a11 })

                12 ->
                    int |> map (\a12 st -> { st | a12 = a12 })

                13 ->
                    int |> map (\a13 st -> { st | a13 = a13 })

                14 ->
                    int |> map (\a14 st -> { st | a14 = a14 })

                15 ->
                    int |> map (\a15 st -> { st | a15 = a15 })

                16 ->
                    int |> map (\a16 st -> { st | a16 = a16 })

                17 ->
                    int |> map (\a17 st -> { st | a17 = a17 })

                18 ->
                    int |> map (\a18 st -> { st | a18 = a18 })

                19 ->
                    int |> map (\a19 st -> { st | a19 = a19 })

                20 ->
                    int |> map (\a20 st -> { st | a20 = a20 })

                21 ->
                    int |> map (\a21 st -> { st | a21 = a21 })

                22 ->
                    int |> map (\a22 st -> { st | a22 = a22 })

                23 ->
                    int |> map (\a23 st -> { st | a23 = a23 })

                24 ->
                    int |> map (\a24 st -> { st | a24 = a24 })

                _ ->
                    fail
        )
        { a0 = 0
        , a1 = 0
        , a2 = 0
        , a3 = 0
        , a4 = 0
        , a5 = 0
        , a6 = 0
        , a7 = 0
        , a8 = 0
        , a9 = 0
        , a10 = 0
        , a11 = 0
        , a12 = 0
        , a13 = 0
        , a14 = 0
        , a15 = 0
        , a16 = 0
        , a17 = 0
        , a18 = 0
        , a19 = 0
        , a20 = 0
        , a21 = 0
        , a22 = 0
        , a23 = 0
        , a24 = 0
        }


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
