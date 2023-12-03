module Bytes.Decode.Branchable exposing
    ( Decoder, decode, fromDecoder
    , succeed, fail
    , unsignedInt8, unsignedInt16, unsignedInt32, signedInt8, signedInt16, signedInt32
    , float32, float64
    , string
    , bytes
    , map, map2, map3, map4, map5
    , keep, ignore, skip
    , andThen, oneOf, repeat, loop
    )

{-| Parse `Bytes` with custom error reporting and context tracking.


# Running the decoder

@docs Decoder, decode, fromDecoder


# Static decoders

@docs succeed, fail


# Basic decoders


## Integers

@docs unsignedInt8, unsignedInt16, unsignedInt32, signedInt8, signedInt16, signedInt32


## Floats

@docs float32, float64


## Strings

@docs string


## Decoding Bytes

@docs bytes


# Transforming values

@docs map, map2, map3, map4, map5


# Combininig decoders

@docs keep, ignore, skip


# Advanced decoders

@docs andThen, oneOf, repeat, loop

-}

import Bytes exposing (Bytes)
import Bytes.Decode as D exposing (Step)


{-| A decoder for a certain type of value.
-}
type Decoder value
    = Decoder (State -> D.Decoder ( State, value ))


type alias State =
    { input : Bytes
    , offset : Int
    }


{-| Run the given decoder on the provided bytes.

    import Bytes.Encode as E
    import Bytes.Decode.Branchable as D

    E.string "hello"
        |> E.encode
        |> D.decode (D.string 5)
    --> Just "hello"

    E.string "hello"
        |> E.encode
        |> D.decode (D.string 6)
    --> Nothing

-}
decode : Decoder value -> Bytes -> Maybe value
decode decoder input =
    runKeepState decoder input
        -- |> Debug.log "(state, value)"
        |> Maybe.map (\( _, value ) -> value)


runKeepState : Decoder value -> Bytes -> Maybe ( State, value )
runKeepState (Decoder decoder) input =
    let
        dec : D.Decoder ( State, value )
        dec =
            decoder { input = input, offset = 0 }
    in
    D.decode dec input


{-| Always succeed with the given value.

    import Bytes.Encode as E
    import Bytes.Decode.Branchable as D

    E.encode (E.sequence [])
        |> D.decode (D.succeed "hi there")
    --> Just "hi there"

-}
succeed : value -> Decoder value
succeed val =
    fromDecoder (D.succeed val) 0


{-| A Decoder that always fails.

    import Bytes.Encode as E
    import Bytes.Decode.Branchable as D

    E.sequence []
        |> E.encode
        |> D.decode D.fail
    --> Nothing

-}
fail : Decoder value
fail =
    fromDecoder D.fail 0


{-| Transform the value a decoder produces

    import Bytes.Encode as E
    import Bytes.Decode.Branchable as D

    E.string "hello"
        |> E.encode
        |> D.decode (D.map String.length (D.string 5))
    --> Just 5

-}
map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder decoder) =
    Decoder <|
        \state ->
            decoder state
                |> D.map (Tuple.mapSecond f)


{-| Combine two decoders into a single one.

    import Bytes exposing (Bytes)
    import Bytes.Encode as E
    import Bytes.Decode.Branchable as D exposing (Decoder)

    input : Bytes
    input =
        [ E.unsignedInt8 2
        , E.string "wat"
        ]
            |> E.sequence
            |> E.encode

    map2Example : Decoder String
    map2Example =
        D.map2 String.repeat D.unsignedInt8 (D.string 3)

    D.decode map2Example input
    --> Just "watwat"

Note that the effect of `map2` (and, in fact, every `map` variation) can also be
achieved using a combination of [`succeed`](#succeed) and [`keep`](#keep).

    equivalent : Decoder String
    equivalent =
        D.succeed String.repeat
            |> D.keep D.unsignedInt8
            |> D.keep (D.string 3)

    D.decode equivalent input
    --> Just "watwat"

-}
map2 : (x -> y -> z) -> Decoder x -> Decoder y -> Decoder z
map2 f decoderX decoderY =
    decoderX |> andThen (\x -> map (\y -> f x y) decoderY)


{-| Combine 3 decoders into a single one.
-}
map3 : (w -> x -> y -> z) -> Decoder w -> Decoder x -> Decoder y -> Decoder z
map3 f decoderW decoderX decoderY =
    map2 f decoderW decoderX
        |> keep decoderY


{-| Combine 4 decoders into a single one.
-}
map4 :
    (v -> w -> x -> y -> z)
    -> Decoder v
    -> Decoder w
    -> Decoder x
    -> Decoder y
    -> Decoder z
map4 f decoderV decoderW decoderX decoderY =
    map3 f decoderV decoderW decoderX
        |> keep decoderY


{-| Combine 5 decoders into a single one.
-}
map5 :
    (u -> v -> w -> x -> y -> z)
    -> Decoder u
    -> Decoder v
    -> Decoder w
    -> Decoder x
    -> Decoder y
    -> Decoder z
map5 f decoderU decoderV decoderW decoderX decoderY =
    map4 f decoderU decoderV decoderW decoderX
        |> keep decoderY


{-| Keep the value produced by a decoder in a pipeline.

Together with [`succeed`](#succeed) and [`ignore`](#ignore), this allows writing
pretty flexible decoders in a straightforward manner: the order in which things
are parsed is apparent.

    import Bytes.Encode as E
    import Bytes.Decode.Branchable as D exposing (Decoder)

    decoder : Decoder (Int, Int)
    decoder =
        D.succeed Tuple.pair
            |> D.keep D.unsignedInt8
            |> D.ignore D.unsignedInt8
            |> D.keep D.unsignedInt8

    [ E.unsignedInt8 12
    , E.unsignedInt8 3
    , E.unsignedInt8 45
    ]
        |> E.sequence
        |> E.encode
        |> D.decode decoder
    --> Just ( 12, 45 )

-}
keep : Decoder a -> Decoder (a -> b) -> Decoder b
keep val fun =
    map2 (<|) fun val


{-| Ignore the value produced by a decoder.

Note that the decoder must still succeed for the pipeline to succeed. This means
you can use this for checking the value of something, without using the value.

    import Bytes.Encode as E
    import Bytes.Decode.Branchable as D exposing (Decoder)

    match : Int -> Decoder Int
    match expected =
        D.unsignedInt8
            |> D.andThen
                (\actual ->
                    if expected == actual then
                        D.succeed actual
                    else
                        D.fail
                )

    decoder : Decoder ()
    decoder =
        D.succeed ()
            |> D.ignore (match 66)

    E.unsignedInt8 66
        |> E.encode
        |> D.decode decoder
    --> Just ()

    E.unsignedInt8 44
        |> E.encode
        |> D.decode decoder
    --> Nothing

-}
ignore : Decoder ignore -> Decoder keep -> Decoder keep
ignore skipper keeper =
    map2 always keeper skipper


{-| Skip a number of bytes in a pipeline.

This is similar to `ignore`, but rather than decoding a value and discarding it,
this just goes ahead and skips them altogether.

-}
skip : Int -> Decoder value -> Decoder value
skip nBytes =
    ignore (bytes nBytes)


{-| Decode one thing, and then another thing based on the first thing.

This is very useful to make the content of your data drive your decoder. As an
example, consider a string encoded as the length of the string, followed by the
actual data:

    import Bytes.Encode as E
    import Bytes.Decode.Branchable as D exposing (Decoder)

    string : Decoder String
    string =
        D.unsignedInt8
            |> D.andThen D.string

    [ E.unsignedInt8 5
    , E.string "hello"
    ]
        |> E.sequence
        |> E.encode
        |> D.decode string
    --> Just "hello"

-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen thenB (Decoder decoderA) =
    Decoder <|
        \state ->
            decoderA state
                |> D.andThen
                    (\( newState, a ) ->
                        let
                            (Decoder decoderB) =
                                thenB a
                        in
                        decoderB newState
                    )


{-| Tries a bunch of decoders and succeeds with the first one to succeed.

All decoder alternatives start at the same location in the bytes.

-}
oneOf : List (Decoder value) -> Decoder value
oneOf options =
    Decoder <|
        \state ->
            oneOfHelper (dropBytes state.offset state.input) options state


oneOfHelper : Bytes -> List (Decoder value) -> State -> D.Decoder ( State, value )
oneOfHelper offsetInput options state =
    case options of
        [] ->
            D.fail

        decoder :: otherDecoders ->
            case runKeepState decoder offsetInput of
                Just ( newState, value ) ->
                    D.bytes newState.offset
                        |> D.map (\_ -> ( { input = state.input, offset = state.offset + newState.offset }, value ))

                Nothing ->
                    oneOfHelper offsetInput otherDecoders state


dropBytes : Int -> Bytes -> Bytes
dropBytes offset bs =
    let
        width : Int
        width =
            Bytes.width bs
    in
    D.map2 (\_ x -> x) (D.bytes offset) (D.bytes <| width - offset)
        |> (\d -> D.decode d bs)
        |> Maybe.withDefault bs


{-| Repeat a given decoder `count` times.

The order of arguments is based on the common occurence of reading the number of
times to repeat something through a decoder.

    import Bytes.Encode as E
    import Bytes.Decode.Branchable as D exposing (Decoder)

    intList : Decoder (List Int)
    intList =
        D.unsignedInt8
            |> D.andThen (D.repeat D.unsignedInt8)

    [ 5, 0, 1, 2, 3, 4 ]
        |> List.map E.unsignedInt8
        |> E.sequence
        |> E.encode
        |> D.decode intList
    --> Just [ 0, 1, 2, 3, 4 ]

-}
repeat : Decoder value -> Int -> Decoder (List value)
repeat p nTimes =
    loop ( nTimes, [] ) (repeatHelp p)


repeatHelp :
    Decoder value
    -> ( Int, List value )
    -> Decoder (Step ( Int, List value ) (List value))
repeatHelp p ( cnt, acc ) =
    if cnt <= 0 then
        succeed (D.Done (List.reverse acc))

    else
        map (\v -> D.Loop ( cnt - 1, v :: acc )) p


{-| Loop a decoder until it declares it is done looping.

The `Step` type in the signature comes from the `Bytes.Decode` module in elm/bytes.
Here is how `repeat` is defined for example.

    import Bytes.Decode
    import Bytes.Decode.Branchable as D exposing (Decoder)

    repeat : Decoder value -> Int -> Decoder (List value)
    repeat p count =
        D.loop ( count, [] ) (repeatHelp p)

    repeatHelp p ( count, acc ) =
        if count <= 0 then
            succeed (Bytes.Decode.Done (List.reverse acc))

        else
            map (\v -> Bytes.Decode.Loop ( count - 1, v :: acc )) p

-}
loop : state -> (state -> Decoder (Step state a)) -> Decoder a
loop initialState callback =
    Decoder <|
        \initialDecoderState ->
            let
                makeDecoderStep : State -> Step state a -> Step ( state, State ) ( State, a )
                makeDecoderStep decoderState step =
                    case step of
                        D.Loop state ->
                            D.Loop ( state, decoderState )

                        D.Done a ->
                            D.Done ( decoderState, a )

                loopStep : ( state, State ) -> D.Decoder (Step ( state, State ) ( State, a ))
                loopStep ( state, decoderState ) =
                    let
                        (Decoder decoder) =
                            callback state
                    in
                    decoder decoderState
                        -- Decoder (State, Step state a)
                        |> D.map (\( newDecoderState, step ) -> makeDecoderStep newDecoderState step)
            in
            D.loop ( initialState, initialDecoderState ) loopStep



-- Basics


{-| Decode one byte into an integer from 0 to 255.
-}
unsignedInt8 : Decoder Int
unsignedInt8 =
    fromDecoder D.unsignedInt8 1


{-| Decode two bytes into an integer from 0 to 65535.
-}
unsignedInt16 : Bytes.Endianness -> Decoder Int
unsignedInt16 bo =
    fromDecoder (D.unsignedInt16 bo) 2


{-| Decode four bytes into an integer from 0 to 4294967295.
-}
unsignedInt32 : Bytes.Endianness -> Decoder Int
unsignedInt32 bo =
    fromDecoder (D.unsignedInt32 bo) 4


{-| Decode one byte into an integer from -128 to 127.
-}
signedInt8 : Decoder Int
signedInt8 =
    fromDecoder D.signedInt8 1


{-| Decode two bytes into an integer from -32768 to 32767.
-}
signedInt16 : Bytes.Endianness -> Decoder Int
signedInt16 bo =
    fromDecoder (D.signedInt16 bo) 2


{-| Decode four bytes into an integer from -2147483648 to 2147483647.
-}
signedInt32 : Bytes.Endianness -> Decoder Int
signedInt32 bo =
    fromDecoder (D.signedInt32 bo) 4


{-| Decode 4 bytes into a Float.
-}
float32 : Bytes.Endianness -> Decoder Float
float32 bo =
    fromDecoder (D.float32 bo) 4


{-| Decode 8 bytes into a Float.
-}
float64 : Bytes.Endianness -> Decoder Float
float64 bo =
    fromDecoder (D.float64 bo) 8


{-| Decode `count` bytes representing UTF-8 characters into a String.

Note that Elm strings use UTF-16. As a result, the `String.length` will not
always agree with the number of bytes that went into it!

    import Bytes.Encode as E
    import Bytes.Decode.Branchable as D

    [ 0xF0, 0x9F, 0x91, 0x8D ]
        |> List.map E.unsignedInt8
        |> E.sequence
        |> E.encode
        |> D.decode (D.string 4)
    --> Just "👍"

-}
string : Int -> Decoder String
string byteCount =
    fromDecoder (D.string byteCount) byteCount


{-| Parse `count` bytes as `Bytes`.
-}
bytes : Int -> Decoder Bytes
bytes count =
    fromDecoder (D.bytes count) count


fromDecoder : D.Decoder v -> Int -> Decoder v
fromDecoder decoder byteLength =
    Decoder <|
        \state ->
            D.map
                (\v -> ( { input = state.input, offset = state.offset + byteLength }, v ))
                decoder
