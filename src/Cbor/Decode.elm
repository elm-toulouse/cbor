module Cbor.Decode exposing
    ( Decoder, decode, maybe
    , bool, int, bigint, float, string, bytes
    , list, length, dict, size, associativeList, fold
    , Step, record, fields, field, optionalField, tuple, elems, elem, optionalElem
    , succeed, fail, andThen, ignoreThen, thenIgnore, map, map2, map3, map4, map5, traverse
    , oneOf, keep, ignore
    , beginString, beginBytes, beginList, beginDict, break
    , tag, tagged
    , any, raw
    )

{-| The Concise Binary Object Representation (CBOR) is a data format whose design
goals include the possibility of extremely small code size, fairly small message
size, and extensibility without the need for version negotiation. These design
goals make it different from earlier binary serializations such as ASN.1 and
MessagePack.


## Decoder

@docs Decoder, decode, maybe


## Primitives

@docs bool, int, bigint, float, string, bytes


## Data Structures

@docs list, length, dict, size, associativeList, fold


## Records & Tuples

@docs Step, record, fields, field, optionalField, tuple, elems, elem, optionalElem


## Mapping

@docs succeed, fail, andThen, ignoreThen, thenIgnore, map, map2, map3, map4, map5, traverse


## Branching

@docs oneOf, keep, ignore


## Streaming

@docs beginString, beginBytes, beginList, beginDict, break


## Tagging

@docs tag, tagged


## Debugging

@docs any, raw

-}

import Bitwise exposing (and, shiftLeftBy, shiftRightBy)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode
import Bytes.Decode.Branchable as D
import Bytes.Encode as E
import Bytes.Floating.Decode as D
import Cbor exposing (CborItem(..), Sign(..))
import Cbor.Encode as CE
import Cbor.Tag exposing (Tag(..))
import Dict exposing (Dict)
import Tuple



{------------------------------------------------------------------------------
                                  Decoder
------------------------------------------------------------------------------}


{-| Describes how to turn a binary CBOR sequence of bytes into any Elm value.
-}
type Decoder a
    = Decoder (D.Decoder Int) (Int -> D.Decoder a)


{-| Turn a binary CBOR sequence of bytes into a nice Elm value.
-}
decode : Decoder a -> Bytes -> Maybe a
decode d =
    D.decode (runDecoder d)


{-| Helpful for dealing with optional items. Turns [`null`](https://package.elm-lang.org/packages/elm-toulouse/cbor/latest/Cbor-Encode#null)
or [`undefined`](https://package.elm-lang.org/packages/elm-toulouse/cbor/latest/Cbor-Encode#undefined) into [`Nothing`](https://package.elm-lang.org/packages/elm/core/latest/Maybe#Maybe).

    D.decode (D.maybe D.bool) Bytes<0xF6> == Just Nothing

    D.decode (D.maybe D.bool) Bytes<0xF4> == Just (Just False)

-}
maybe : Decoder a -> Decoder (Maybe a)
maybe (Decoder consumeNext processNext) =
    Decoder consumeNext <|
        \a ->
            if a == 0xF6 || a == 0xF7 then
                D.succeed Nothing

            else
                D.map Just (processNext a)



{-------------------------------------------------------------------------------
                                 Primitives
-------------------------------------------------------------------------------}


{-| Decode a boolean.

    D.decode D.bool Bytes<0xF4> == Just False

    D.decode D.bool Bytes<0xF5> == Just True

-}
bool : Decoder Bool
bool =
    consumeNextMajor 7 <|
        \a ->
            if a == 20 then
                D.succeed False

            else if a == 21 then
                D.succeed True

            else
                D.fail


{-| Decode an integer. Note that there's no granular decoders (nor encoders)
because, the CBOR encoding of an integers varies depending on the integer value.
Therefore, at the CBOR-level, as in Elm, there's no notion of _smaller_ integers.

    D.decode D.int Bytes<0x00> == Just 0

    D.decode D.int Bytes<0x00> == Just 0

    D.decode D.int Bytes<0x1A, 0x00, 0x02, 0x33, 0x56> == Just 1337

    D.decode D.int Bytes<0x2D> == Just -14

-}
int : Decoder Int
int =
    -- NOTE Unfortunately, we don't have any 'Alternative'-ish instance on
    -- @Byte.Decoder@, or something like 'oneOf' to try several decoders in
    -- sequence. Since Elm conflates representation of unsigned and negative
    -- integer into one 'int' type, we have to define an ad-hoc decoder for
    -- the major types here to handle both the Major type 0 and 1.
    Decoder D.unsignedInt8
        (\a ->
            if shiftRightBy 5 a == 0 then
                unsigned a

            else if shiftRightBy 5 a == 1 then
                D.map (\x -> negate x - 1) (unsigned (and a 31))

            else
                D.fail
        )


{-| Internal usage only for CborItem decoding of 64-bit integers.
We keep track of the integer sign in the most significant bytes part.
-}
int64 : Int -> D.Decoder ( Int, Int )
int64 major =
    D.map2 Tuple.pair (D.unsignedInt32 BE) (D.unsignedInt32 BE)
        |> D.map
            (\( msb, lsb ) ->
                if major == 0 then
                    -- Positive integer, we keep msb and lsb positive
                    ( msb, lsb )

                else if lsb == 0xFFFFFFFF then
                    -- Negative integer, at the 2^32 frontier
                    ( negate (msb + 1), 0 )

                else
                    -- Negative integer
                    ( negate msb, lsb + 1 )
            )


{-| Decode an unbounded integer as a big-endian bytes sequence. This
is particularly useful for decoding large integer values which are beyond the
max and min safe integer values allowed by Elm.

This can also be used to decode tagged [`PositiveBigNum`](../Cbor-Tag#Tag) and [`NegativeBigNum`](../Cbor-Tag#Tag)
integer values following the tag semantic specified in [RFC-7049](https://www.rfc-editor.org/rfc/rfc7049#appendix-B).

    -- 0
    D.decode D.bigint Bytes<0x00>
        == Just (Positive, Bytes<0x00>)

    -- 1337
    D.decode D.bigint Bytes<0x19, 0x05, 0x39>
        == Just (Positive, Bytes<0x05, 0x39>)

    -- -14
    D.decode D.bigint Bytes<0x2D>
        == Just (Negative, Bytes<0x0E>)

    -- -1536
    D.decode D.bigint Bytes<0x39, 0x05, 0xFF>
        == Just <Negative, Bytes<0x06, 0x00>

    -- 2^53
    D.decode D.bigint Bytes<0x1B, 0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>
        == Just (Positive, Bytes<0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>)

    -- 2^64
    D.decode D.bigint Bytes<0xC2, 0x49, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>
        == Just (Positive, Bytes<0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>)

-}
bigint : Decoder ( Sign, Bytes )
bigint =
    let
        positive =
            D.map (\n -> ( Positive, n ))

        negative =
            D.map (\n -> ( Negative, n ))

        -- Increment the bytes by +1
        -- Adjust the bytes array size
        increment : Bytes -> D.Decoder Bytes
        increment bs =
            let
                width =
                    Bytes.width bs
            in
            bs
                |> D.decode
                    (D.loop ( width, [] )
                        (\( sz, xs ) ->
                            if sz > 0 then
                                D.unsignedInt8 |> D.map (\x -> Bytes.Decode.Loop ( sz - 1, x :: xs ))

                            else
                                D.succeed (Bytes.Decode.Done xs)
                        )
                    )
                |> Maybe.map
                    (List.foldl
                        (\x ( done, ys ) ->
                            if done then
                                ( done
                                , E.unsignedInt8 x :: ys
                                )

                            else if x >= 255 then
                                ( done
                                , E.unsignedInt8 0 :: ys
                                )

                            else
                                ( True
                                , E.unsignedInt8 (x + 1) :: ys
                                )
                        )
                        ( False, [] )
                        >> (\( done, xs ) ->
                                if done then
                                    xs

                                else
                                    -- NOTE: For consistency, if we have to add an extra byte
                                    -- because we are at a boundary, we choose to still return 2, 4
                                    -- or 8 bytes for "small" values, padded-left with zeroes.
                                    -- This way, the API is somewhat symmetric between positive and
                                    -- negative numbers.
                                    case width of
                                        2 ->
                                            E.unsignedInt8 0 :: E.unsignedInt8 1 :: xs

                                        4 ->
                                            E.unsignedInt8 0 :: E.unsignedInt8 0 :: E.unsignedInt8 0 :: E.unsignedInt8 1 :: xs

                                        _ ->
                                            E.unsignedInt8 1 :: xs
                           )
                        >> E.sequence
                        >> E.encode
                        >> D.succeed
                    )
                |> Maybe.withDefault D.fail
    in
    Decoder D.unsignedInt8
        (\a ->
            case shiftRightBy 5 a of
                0 ->
                    -- positive int
                    payloadForMajor 0 a
                        |> D.andThen unsignedBytes
                        |> positive

                1 ->
                    -- negative int
                    payloadForMajor 1 a
                        |> D.andThen unsignedBytes
                        |> D.andThen increment
                        |> negative

                6 ->
                    -- tagged value
                    let
                        (Decoder _ processTag) =
                            tag
                    in
                    processTag a
                        |> D.andThen
                            (\t ->
                                case t of
                                    PositiveBigNum ->
                                        runDecoder bytes |> positive

                                    NegativeBigNum ->
                                        runDecoder bytes
                                            |> D.andThen increment
                                            |> negative

                                    _ ->
                                        D.fail
                            )

                _ ->
                    D.fail
        )


{-| Decode a floating point number, regardless of its precision. Because in Elm,
there exists only one 'Float' type, which is encoded in double-precision, we do
decode all floats into this double-precision number in Elm which may lead to
some precision gain and therefore, different representation than one would
expect.

    D.decode D.float <| E.encode (E.float16 1.1) == Just 1.099609375

    D.decode D.float <| E.encode (E.float64 1.1) == Just 1.1

-}
float : Decoder Float
float =
    consumeNextMajor 7 <|
        \a ->
            if a == 25 then
                D.fromDecoder (D.float16 BE) 2

            else if a == 26 then
                D.float32 BE

            else if a == 27 then
                D.float64 BE

            else
                D.fail


{-| Decode a bunch UTF-8 bytes into a [`String`](https://package.elm-lang.org/packages/elm/core/latest/String#String).
In case of streaming, all string chunks are decoded at once and concatenated as if they were
one single string. See also [`Cbor.Encode.beginString`](https://package.elm-lang.org/packages/elm-toulouse/cbor/latest/Cbor-Encode#beginString).
-}
string : Decoder String
string =
    chunks 3 D.string String.concat


{-| Decode a bunch bytes into [`Bytes`](https://package.elm-lang.org/packages/elm/bytes/latest/Bytes#Bytes).
In case of streaming, all byte chunks are decoded at once and concatenated as if they were
one single byte string. See also [`Cbor.Encode.beginBytes`](https://package.elm-lang.org/packages/elm-toulouse/cbor/latest/Cbor-Encode#beginBytes).
-}
bytes : Decoder Bytes
bytes =
    chunks 2 D.bytes (List.map E.bytes >> E.sequence >> E.encode)


{-| Decode string-like structures ensuring that long strings split over multiple
chunks are aggregated
-}
chunks :
    Int
    -> (Int -> D.Decoder str)
    -> (List str -> str)
    -> Decoder str
chunks majorType chunk mappend =
    let
        indef es =
            D.unsignedInt8
                |> D.andThen
                    (\a ->
                        if a == tBREAK then
                            es
                                |> List.reverse
                                |> mappend
                                |> Bytes.Decode.Done
                                |> D.succeed

                        else
                            payloadForMajor majorType a
                                |> D.andThen unsigned
                                |> D.andThen chunk
                                |> D.map (\e -> Bytes.Decode.Loop (e :: es))
                    )
    in
    consumeNextMajor majorType <|
        \a ->
            if a == tBEGIN then
                D.loop [] indef

            else
                unsigned a |> D.andThen chunk



{-------------------------------------------------------------------------------
                              Data-Structures
-------------------------------------------------------------------------------}


{-| Decode a `List` of items `a`. The list can be definite or indefinite.

    D.decode (D.list D.int) Bytes<0x82, 0x0E, 0x18, 0x2A> == Just [ 14, 42 ]

    D.decode (D.list D.int) Bytes<0x9F, 0x01, 0x02, 0xFF> == Just [ 1, 1 ]

    D.decode (D.list (D.list D.bool)) Bytes<0x81, 0x9F, 0xF4, 0xFF> == Just [ [ False ] ]

-}
list : Decoder a -> Decoder (List a)
list (Decoder consumeNext processNext) =
    foldable 4 consumeNext processNext


{-| Decode only the length of a (definite) CBOR array.
-}
length : Decoder Int
length =
    definiteLength 4


{-| Decode an CBOR map of pairs of data-items into an Elm [`Dict`](https://package.elm-lang.org/packages/elm/core/latest/Dict#Dict).
The map can be either of definite length or indefinite length.

    D.decode (D.dict D.int D.int) Bytes<0xA1, 0x0E, 0x18, 0x2A>
        == Dict.fromList [ ( 14, 42 ) ]

    D.decode (D.dict D.string D.int) Bytes<0xBF, 0x61, 0x61, 0x0E, 0x61, 0x62, 0x18, 0x2A, 0xFF>
        == Dict.fromList [ ( "a", 14 ), ( "b", 42 ) ]

-}
dict : Decoder comparable -> Decoder a -> Decoder (Dict comparable a)
dict key value =
    map Dict.fromList <| associativeList key value


{-| Decode only the size of a (definite) CBOR map.
-}
size : Decoder Int
size =
    definiteLength 5


{-| Decode a CBOR map as an associative list, where keys and values can be arbitrary CBOR.
If keys are [`comparable`](https://package.elm-lang.org/packages/elm/core/latest/Basics#comparison),
you should prefer [`dict`](#dict) to this primitive.
-}
associativeList : Decoder k -> Decoder v -> Decoder (List ( k, v ))
associativeList (Decoder consumeNextKey processNextKey) value =
    foldable 5
        consumeNextKey
        (\key ->
            D.map2
                Tuple.pair
                (processNextKey key)
                (runDecoder value)
        )


{-| Decode a CBOR map by folding over its entries and updating a state variable.
Useful to decode record-like structures that have empty values.

    type alias Foo =
        { foo : Maybe Int
        , bar : Maybe Bool
        }

    decodeFoo : D.Decoder Foo
    decodeFoo =
        D.fold D.int
            (\k ->
                case k of
                    0 ->
                        D.int |> D.andThen (\v st -> { st | foo = Just v })

                    1 ->
                        D.bool |> D.andThen (\v st -> { st | bar = Just v })

                    _ ->
                        D.fail
            )
            { foo = Nothing, bar = Nothing }

-}
fold :
    Decoder k
    -> (k -> Decoder (state -> state))
    -> state
    -> Decoder state
fold (Decoder consumeNextKey processNextKey) stepDecoder initialState =
    let
        indef : state -> D.Decoder (Bytes.Decode.Step state state)
        indef state =
            consumeNextKey
                |> D.andThen
                    (\a ->
                        if a == tBREAK then
                            D.succeed (Bytes.Decode.Done state)

                        else
                            processNextKey a
                                |> D.andThen (\k -> runDecoder <| stepDecoder k)
                                |> D.map (\f -> Bytes.Decode.Loop (f state))
                    )

        def : ( Int, state ) -> D.Decoder (Bytes.Decode.Step ( Int, state ) state)
        def ( n, state ) =
            if n <= 0 then
                D.succeed (Bytes.Decode.Done state)

            else
                consumeNextKey
                    |> D.andThen processNextKey
                    |> D.andThen (\k -> runDecoder <| stepDecoder k)
                    |> D.map (\f -> Bytes.Decode.Loop ( n - 1, f state ))
    in
    consumeNextMajor 5 <|
        \a ->
            if a == tBEGIN then
                D.loop initialState indef

            else
                unsigned a
                    |> D.andThen (\n -> D.loop ( n, initialState ) def)


{-| A re-usable generic encoder for list-like data structures. This decoder
ensures to seamlessly decode definite and indefinite structures.
-}
foldable :
    Int
    -> D.Decoder Int
    -> (Int -> D.Decoder a)
    -> Decoder (List a)
foldable majorType consumeNext processNext =
    let
        indef es =
            consumeNext
                |> D.andThen
                    (\a ->
                        if a == tBREAK then
                            es
                                |> List.reverse
                                |> Bytes.Decode.Done
                                |> D.succeed

                        else
                            processNext a
                                |> D.map (\e -> Bytes.Decode.Loop (e :: es))
                    )

        def ( n, es ) =
            if n <= 0 then
                es |> List.reverse |> Bytes.Decode.Done |> D.succeed

            else
                consumeNext
                    |> D.andThen processNext
                    |> D.map (\e -> Bytes.Decode.Loop ( n - 1, e :: es ))
    in
    consumeNextMajor majorType <|
        \a ->
            if a == tBEGIN then
                D.loop [] indef

            else
                unsigned a |> D.andThen (\n -> D.loop ( n, [] ) def)


{-| A decoder for definite length or size.
-}
definiteLength : Int -> Decoder Int
definiteLength majorType =
    consumeNextMajor majorType <|
        \a ->
            if a == tBEGIN then
                D.fail

            else
                unsigned a



{-------------------------------------------------------------------------------
                                 Record / Tuples
-------------------------------------------------------------------------------}


{-| An intermediate (opaque) step in the decoding of a record. See
[`record`](#record) or [`tuple`](#tuple) for more detail.
-}
type Step k result
    = TupleStep (TupleSt k result)
    | RecordStep (RecordSt k result)


type Size
    = Definite Int
    | Indefinite Bool


type alias TupleSt k result =
    { size : Size
    , steps : result
    , decodeKey : Decoder k
    , k : Maybe k
    }


type alias RecordSt k result =
    { steps : result
    , rest : List ( k, Bytes )
    }


{-| Ideally, we would use two types for 'Step', but:

(a) It causes a breaking change in the current API
(b) It makes the API harder to decipher as there are now two opaque internal 'State' for both tuples and record.

So we hide a bit of that complexity to the users and keep it internal. Since Elm
doesn't support GADTs, we can't discriminate at the type-level and must resort
to a little helper function to pattern-match.

-}
withTupleStep :
    (TupleSt k (field -> steps) -> Decoder (TupleSt k steps))
    -> Decoder (Step k (field -> steps))
    -> Decoder (Step k steps)
withTupleStep with st =
    st
        |> andThen
            (\inner ->
                case inner of
                    TupleStep t ->
                        map TupleStep (with t)

                    RecordStep _ ->
                        fail
            )


{-| See [withTupleStep](#withTupleStep).
-}
withRecordStep :
    (RecordSt k (field -> steps) -> Decoder (RecordSt k steps))
    -> Decoder (Step k (field -> steps))
    -> Decoder (Step k steps)
withRecordStep with st =
    st
        |> andThen
            (\inner ->
                case inner of
                    TupleStep _ ->
                        fail

                    RecordStep r ->
                        map RecordStep (with r)
            )


{-| Decode a (definite) CBOR map into a record. Keys in the map can be arbitrary
CBOR but are expected to be homogeneous across the record.

    type alias Album =
        { artist : String
        , title : String
        , label : Maybe String
        }

    -- In this example, we use compact integer as keys.
    decodeAlbumCompact : D.Decoder Album
    decodeAlbumCompact =
        D.record D.int Album <|
            D.fields
                >> D.field 0 D.string
                >> D.field 1 D.string
                >> D.optionalField 2 D.string

    -- In this example, we use more verbose string keys.
    decodeAlbumVerbose : D.Decoder Album
    decodeAlbumVerbose =
        D.record D.string Album <|
            D.fields
                >> D.field "artist" D.string
                >> D.field "title" D.string
                >> D.optionalField "label" D.string

-}
record : Decoder k -> steps -> (Step k steps -> Decoder (Step k record)) -> Decoder record
record decodeKey steps decodeNext =
    associativeList decodeKey raw
        |> andThen (\rest -> RecordStep { steps = steps, rest = rest } |> decodeNext)
        |> andThen
            (\result ->
                case result of
                    RecordStep st ->
                        succeed st.steps

                    TupleStep _ ->
                        fail
            )


{-| A helper that makes writing record decoders nicer. It is equivalent to
[`succeed`](#succeed), but let us align decoders to fight compulsory OCDs.
-}
fields : Step k steps -> Decoder (Step k steps)
fields =
    succeed


{-| Decode a field of record and step through the decoder. This ensures that
the decoded key matches the expected key. If it doesn't, the entire decoder is failed.

See [`record`](#record) for detail about usage.

-}
field : k -> Decoder field -> Decoder (Step k (field -> steps)) -> Decoder (Step k steps)
field want decodeField =
    withRecordStep <|
        \st ->
            extract want st.rest
                |> Maybe.andThen
                    (\( bs, rest ) ->
                        decode decodeField bs
                            |> Maybe.map (\v -> succeed { rest = rest, steps = st.steps v })
                    )
                |> Maybe.withDefault fail


{-| Decode an optional field of record and step through the decoder. This
ensures that the decoded key matches the expected key. If it doesn't, the entire
decoder is failed. When the key is missing, returns `Nothing` as a value.

See [`record`](#record) for detail about usage.

-}
optionalField : k -> Decoder field -> Decoder (Step k (Maybe field -> steps)) -> Decoder (Step k steps)
optionalField want decodeField =
    withRecordStep <|
        \st ->
            case extract want st.rest of
                Just ( bs, rest ) ->
                    decode decodeField bs
                        |> Maybe.map (\v -> succeed { rest = rest, steps = st.steps (Just v) })
                        |> Maybe.withDefault fail

                Nothing ->
                    succeed { rest = st.rest, steps = st.steps Nothing }


{-| Decode a (definite) CBOR array into a record / tuple.

    type alias Track =
        { title : String
        , duration : Int
        }

    decodeTrack : D.Decoder Track
    decodeTrack =
        D.tuple Track <|
            D.elems
                >> D.elem D.string
                >> D.elem D.int

-}
tuple : steps -> (Step Never steps -> Decoder (Step Never tuple)) -> Decoder tuple
tuple steps decodeTuple =
    consumeNextMajor 4 <|
        \tFirst ->
            if tFirst == tBEGIN then
                TupleStep
                    { k = Nothing
                    , steps = steps
                    , size = Indefinite False
                    , decodeKey = fail
                    }
                    |> decodeTuple
                    |> andThen
                        (\result ->
                            case result of
                                RecordStep _ ->
                                    fail

                                TupleStep st ->
                                    -- When decoding indefinite-length *tuples*, we
                                    -- don't always decode the final break byte. So we must
                                    -- ensure to do it here.
                                    case st.size of
                                        Indefinite True ->
                                            succeed st.steps

                                        _ ->
                                            Decoder D.unsignedInt8 <|
                                                \tLast ->
                                                    if tLast == tBREAK then
                                                        D.succeed st.steps

                                                    else
                                                        D.fail
                        )
                    |> runDecoder

            else
                unsigned tFirst
                    |> D.andThen
                        (\sz ->
                            TupleStep
                                { k = Nothing
                                , steps = steps
                                , size = Definite sz
                                , decodeKey = fail
                                }
                                |> decodeTuple
                                |> runDecoder
                        )
                    |> D.andThen
                        (\result ->
                            case result of
                                RecordStep _ ->
                                    D.fail

                                TupleStep st ->
                                    D.succeed st.steps
                        )


{-| A helper that makes writing record decoders nicer. It is equivalent to
[`succeed`](#succeed), but let us align decoders to fight compulsory OCDs.
-}
elems : Step Never steps -> Decoder (Step Never steps)
elems =
    succeed


{-| Decode an element of a record or tuple and step through the decoder.

See [`tuple`](#tuple) for detail about usage.

-}
elem : Decoder field -> Decoder (Step Never (field -> steps)) -> Decoder (Step Never steps)
elem v =
    withTupleStep <| \st -> map (step st Nothing) v


{-| Decode an optional element of a record or tuple and step through the
decoder. Note that optional elements only make sense at the end of a structure.

In particular, optional elements must be contiguous and can only be optional
(resp. missing) if all their successors are also optional (resp. missing).

For example, consider:

    type alias Foo =
        Foo
            { a : Int
            , b : Maybe Int
            , c : Maybe Int
            }

    decodeFoo =
        tuple Foo <|
            elems
                >> elem int
                >> optionalElem int
                >> optionalElem int

  - It is possible for `c` to be missing.
  - It is possible for `b` and `c` to be missing.
  - But it isn't possible for only `b` to be missing as this is undistinguishable from `c` missing.

If you need such behavior, use [`elem`](#elem) with [`maybe`](#maybe).

-}
optionalElem : Decoder field -> Decoder (Step Never (Maybe field -> steps)) -> Decoder (Step Never steps)
optionalElem v =
    withTupleStep <|
        \st ->
            let
                ignoreElem =
                    step st st.k Nothing
            in
            case st.size of
                Indefinite done ->
                    if done then
                        succeed ignoreElem

                    else
                        let
                            (Decoder consumeField processField) =
                                v
                        in
                        Decoder consumeField <|
                            \t ->
                                if t == tBREAK then
                                    D.succeed <| { ignoreElem | size = Indefinite True }

                                else
                                    D.map (step st Nothing << Just) (processField t)

                Definite sz ->
                    if sz <= 0 then
                        succeed ignoreElem

                    else
                        map (step st Nothing << Just) v


{-| Internal, generic stepping function
-}
step : TupleSt k (field -> steps) -> Maybe k -> field -> TupleSt k steps
step st k next =
    { k = k
    , steps = st.steps next
    , decodeKey = st.decodeKey
    , size =
        case st.size of
            Indefinite done ->
                Indefinite done

            Definite sz ->
                -- NOTE: We need to count the total size to know whether we have any
                -- element left to parse. This is because a record may contain
                -- optional fields and we don't want to even try to parse the next
                -- key if we have parsed all the required fields.
                --
                -- This is only possible because the size of the record is declared
                -- next to the major type; so before we begin parsing any field we
                -- know how many we expect.
                Definite <|
                    sz
                        - (case k of
                            -- k == Nothing, means that we have consumed the next field.
                            Nothing ->
                                1

                            -- k == Just _, means that we haven't consumed the field and
                            -- we are holding on the key. Waiting for a matching field.
                            Just _ ->
                                0
                          )
    }



{-------------------------------------------------------------------------------
                                  Tagging
-------------------------------------------------------------------------------}


{-| This implementation does little interpretation of the tags and is limited to
only decoding the tag's value. The tag's payload has to be specified as any
other decoder. So, using the previous example, one could decode a bignum as:

    D.decode (D.tag |> D.andThen (\tag -> D.bytes)) input

You may also use [`tagged`](#tagged) if you as a helper to decode a tagged value, while
verifying that the tag matches what you expect.

    D.decode (D.tagged PositiveBigNum D.bytes) input

-}
tag : Decoder Tag
tag =
    consumeNextMajor 6 <|
        unsigned
            >> D.map
                (\t ->
                    case t of
                        0 ->
                            StandardDateTime

                        1 ->
                            EpochDateTime

                        2 ->
                            PositiveBigNum

                        3 ->
                            NegativeBigNum

                        4 ->
                            DecimalFraction

                        5 ->
                            BigFloat

                        21 ->
                            Base64UrlConversion

                        22 ->
                            Base64Conversion

                        23 ->
                            Base16Conversion

                        24 ->
                            Cbor

                        32 ->
                            Uri

                        33 ->
                            Base64Url

                        34 ->
                            Base64

                        35 ->
                            Regex

                        36 ->
                            Mime

                        55799 ->
                            IsCbor

                        _ ->
                            Unknown t
                )


{-| Decode a value that is tagged with the given [`Tag`](https://package.elm-lang.org/packages/elm-toulouse/cbor/latest/Cbor-Tag#Tag).
Fails if the value is not tagged, or, tagged with some other [`Tag`](https://package.elm-lang.org/packages/elm-toulouse/cbor/latest/Cbor-Tag#Tag)

    D.decode (D.tagged Cbor D.int) Bytes<0xD8, 0x0E> == Just ( Cbor, 14 )

    D.decode (D.tagged Base64 D.int) Bytes<0xD8, 0x0E> == Nothing

    D.decode (D.tagged Cbor D.int) Bytes<0x0E> == Nothing

-}
tagged : Tag -> Decoder a -> Decoder ( Tag, a )
tagged t a =
    tag
        |> andThen
            (\t_ ->
                if t == t_ then
                    map2 Tuple.pair (succeed t) a

                else
                    fail
            )



{-------------------------------------------------------------------------------
                                  Mapping
-------------------------------------------------------------------------------}


{-| Always succeed to produce a certain Elm value.

    D.decode (D.succeed 14) Bytes<0x42, 0x28> == Just 14

    D.decode (D.list (D.int |> D.andThen (\_ -> D.succeed 1))) Bytes<0x83, 0x00, 0x00, 0x00>
        == Just [ 1, 1, 1 ]

This particularly handy when used in combination with [`andThen`](#andThen).

-}
succeed : a -> Decoder a
succeed a =
    let
        absurd =
            shiftLeftBy 5 28
    in
    Decoder (D.succeed absurd) (always <| D.succeed a)


{-| A decoder that always fail.

    D.decode D.fail Bytes<0x00> == Nothing

This is particularly handy when used in combination with [`andThen`](#andThen)

-}
fail : Decoder a
fail =
    -- NOTE: We must decode at least one byte here to avoid failing too early
    -- with the 'fail' decoder. This is needed for example for the 'maybe'
    -- combinator which will only execute 'processNext' when it encounters a
    -- non-null CBOR byte. So we must not fail when consuming the next byte, but
    -- only later when processing it.
    --
    -- This is okay because:
    --
    -- (a) If there's no more byte to consume, we still fail as expected.
    -- (b) This cannot lead to weird encoding following D.fail because nothing
    -- can follow D.fail
    Decoder D.unsignedInt8 (always D.fail)


{-| Decode something and then use that information to decode something else.
This is useful when a ['Decoder'](#Decoder) depends on a value held by another decoder:

    tagged : Tag -> Decoder a -> Decoder a
    tagged expectedTag decodeA =
        tag
            |> andThen
                (\decodedTag ->
                    if decodedTag == expectedTag then
                        decodeA

                    else
                        fail
                )

-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen fn (Decoder consumeNext processNext) =
    Decoder
        consumeNext
        (processNext >> D.andThen (fn >> runDecoder))


{-| Decode something and then another thing, but only continue with the second
result.

    ignoreThen a ignored =
        ignored |> andThen (always a)

-}
ignoreThen : Decoder a -> Decoder ignored -> Decoder a
ignoreThen a ignored =
    ignored |> andThen (always a)


{-| Decode something and then another thing, but only continue with the first
result.

    thenIgnore ignored a =
        a |> andThen (\result -> map (always result) ignored)

-}
thenIgnore : Decoder ignored -> Decoder a -> Decoder a
thenIgnore ignored a =
    a |> andThen (\result -> map (always result) ignored)


{-| Transform a decoder. For example, maybe you just want to know the length of a string:

    import String

    stringLength : Decoder Int
    stringLength =
        D.map String.length D.string

-}
map : (a -> value) -> Decoder a -> Decoder value
map fn (Decoder consumeNext processNext) =
    Decoder
        consumeNext
        (processNext >> D.map fn)


{-| Try two decoders and then combine the result. Can be used to decode objects
with many fields:

    type alias Point =
        { x : Float, y : Float }

    point : Decoder Point
    point =
        D.map2 Point D.float D.float

It tries each individual decoder and puts the result together with the Point
constructor.

-}
map2 : (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
map2 fn (Decoder consumeNext processNext) b =
    Decoder consumeNext
        (processNext
            >> (\a ->
                    D.map2 fn
                        a
                        (runDecoder b)
               )
        )


{-| Try three decoders and then combine the result. Can be used to decode
objects with many fields:

    type alias Person =
        { name : String, age : Int, height : Float }

    person : Decoder Person
    person =
        D.map3 Person D.string D.int D.float

Like `map2` it tries each decoder in order and then give the results to the
`Person` constructor. That can be any function though!

-}
map3 :
    (a -> b -> c -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder value
map3 fn (Decoder consumeNext processNext) b c =
    Decoder consumeNext
        (processNext
            >> (\a ->
                    D.map3 fn
                        a
                        (runDecoder b)
                        (runDecoder c)
               )
        )


{-| Try four decoders and then combine the result. See also 'map3' for some
examples.
-}
map4 :
    (a -> b -> c -> d -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder value
map4 fn (Decoder consumeNext processNext) b c d =
    Decoder consumeNext
        (processNext
            >> (\a ->
                    D.map4 fn
                        a
                        (runDecoder b)
                        (runDecoder c)
                        (runDecoder d)
               )
        )


{-| Try five decoders and then combine the result. See also 'map3' for some
examples.
-}
map5 :
    (a -> b -> c -> d -> e -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder value
map5 fn (Decoder consumeNext processNext) b c d e =
    Decoder consumeNext
        (processNext
            >> (\a ->
                    D.map5 fn
                        a
                        (runDecoder b)
                        (runDecoder c)
                        (runDecoder d)
                        (runDecoder e)
               )
        )


{-| Apply a decoder to all elements of a `List`, in order, and yield the
resulting resulting `List`.
-}
traverse : (a -> Decoder b) -> List a -> Decoder (List b)
traverse fn =
    List.foldr
        (\a st ->
            fn a
                |> andThen (\b -> map (\bs -> b :: bs) st)
        )
        (succeed [])



{-------------------------------------------------------------------------------
                                 Branching
-------------------------------------------------------------------------------}


{-| Decode something that can have one of many shapes without prior information
on the correct shape.

    type IntOrString
        = IntVariant Int
        | StringVariant String

    intOrString : Decoder IntOrString
    intOrString =
        D.oneOf [ D.map IntVariant D.int, D.map StringVariant D.string ]

    Bytes<0x64, 0xF0, 0x9F, 0x8C, 0x88>
        |> D.decode intOrString
    --> (Just <| StringVariant "🌈")

-}
oneOf : List (Decoder a) -> Decoder a
oneOf alternatives =
    let
        absurd =
            shiftLeftBy 5 28
    in
    Decoder (D.oneOf [ D.peek, D.succeed absurd ]) <|
        (alternatives
            |> List.map runDecoder
            |> D.oneOf
            |> always
        )


{-| Decode the next value from a structure, and continue decoding.

This is particularly useful when manually decoding an array-like or map-like
structure that materialize multi-variant constructors. For example, imagine the
following:

    type Hostname
        = Single Ipv4 TcpPort
        | Multi AaaRecord TcpPort
        | Dns SrvRecord

Which could be encoded as such:

    toCBOR : Hostname -> E.Encoder
    toCBOR host =
        case host of
            Single ip tcpPort ->
                E.sequence
                    [ E.length 3
                    , E.int 0
                    , Ipv4.toCbor ip
                    , TcpPort.toCbor tcpPort
                    ]

            Multi record tcpPort ->
                E.sequence
                    [ E.length 3
                    , E.int 1
                    , AaaRecord.toCbor record
                    , TcpPort.toCbor tcpPort
                    ]

            Dns record ->
                E.sequence
                    [ E.length 2
                    , E.int 2
                    , SrvRecord.toCbor record
                    ]

Here, the usual [`tuple`](#tuple) helper would be hopeless since we can't commit
to a single constructor a priori. Using [`oneOf`](#oneOf) is also unsatisfactory
as we could branch into the right decoder by just peaking at the first byte.
Using `keep` and [`ignore`](#ignore) we can write an efficient decoder as such:

    fromCbor : D.Decoder Hostname
    fromCbor =
        D.length
            |> D.andThen (\len -> D.map (\tag -> ( len, tag )) D.int)
            |> D.andThen
                (\( _, tag ) ->
                    case tag of
                        0 ->
                            succeed Single
                                |> keep Ipv4.fromCbor
                                |> keep TcpPort.fromCbor

                        1 ->
                            succeed Multi
                                |> keep AaaRecord.fromCbor
                                |> keep TcpPort.fromCbor

                        2 ->
                            succeed Dns
                                |> SrvRecord.fromCbor

                        _ ->
                            D.fail
                )

-}
keep : Decoder a -> Decoder (a -> b) -> Decoder b
keep val fun =
    map2 (<|) fun val


{-| Ignore a field when manually decoding a structure. See [`keep`](#keep) for
details
-}
ignore : Decoder ignore -> Decoder keep -> Decoder keep
ignore skipper keeper =
    map2 always keeper skipper



{-------------------------------------------------------------------------------
                                 Streaming
-------------------------------------------------------------------------------}


{-| Decode the beginning of an indefinite [`Bytes`](https://package.elm-lang.org/packages/elm/bytes/latest/Bytes#Bytes) sequence.

This is useful in combination with [`ignoreThen`](#ignoreThen) and
[`andThen`](#andThen) to manually decode sequences of byte strings.

-}
beginBytes : Decoder ()
beginBytes =
    consumeNextMajor 2 <|
        \a ->
            if a == tBEGIN then
                D.succeed ()

            else
                D.fail


{-| Decode the beginning of an indefinite [`String`](https://package.elm-lang.org/packages/elm/core/latest/String#String) sequence.

This is useful in combination with [`ignoreThen`](#ignoreThen) and
[`andThen`](#andThen) to manually decode sequences of text strings.

-}
beginString : Decoder ()
beginString =
    consumeNextMajor 3 <|
        \a ->
            if a == tBEGIN then
                D.succeed ()

            else
                D.fail


{-| Decode the beginning of an indefinite `List`.

This is useful in combination with [`ignoreThen`](#ignoreThen) and
[`andThen`](#andThen) to manually decode indefinite `List`.

-}
beginList : Decoder ()
beginList =
    consumeNextMajor 4 <|
        \a ->
            if a == tBEGIN then
                D.succeed ()

            else
                D.fail


{-| Decode the beginning of an indefinite [`Dict`](https://package.elm-lang.org/packages/elm/core/latest/Dict#Dict).

This is useful in combination with [`ignoreThen`](#ignoreThen) and
[`andThen`](#andThen) to manually decode indefinite [`Dict`](https://package.elm-lang.org/packages/elm/core/latest/Dict#Dict).

-}
beginDict : Decoder ()
beginDict =
    consumeNextMajor 5 <|
        \a ->
            if a == tBEGIN then
                D.succeed ()

            else
                D.fail


{-| Decode a termination of an indefinite structure. See
[`beginString`](#beginString), [`beginBytes`](#beginBytes),
[`beginList`](#beginList), [`beginDict`](#beginDict) for detail about usage.
-}
break : Decoder ()
break =
    Decoder D.unsignedInt8 <|
        \a ->
            if a == tBREAK then
                D.succeed ()

            else
                D.fail



{-------------------------------------------------------------------------------
                                  Debugging
-------------------------------------------------------------------------------}


{-| Decode remaining bytes as _any_ [`CborItem`](https://package.elm-lang.org/packages/elm-toulouse/cbor/latest/Cbor#CborItem). This is useful
for debugging or to inspect some unknown Cbor data.

    D.decode D.any <| E.encode (E.int 14) == Just (CborInt 14)

-}
any : Decoder CborItem
any =
    Decoder D.unsignedInt8 <|
        \a ->
            let
                majorType =
                    shiftRightBy 5 a

                payload =
                    and a 31

                apply : Decoder a -> Int -> D.Decoder a
                apply (Decoder _ processNext) i =
                    processNext i
            in
            if majorType == 0 then
                -- Check if 64-bit integer
                if payload == 27 then
                    D.map CborInt64 <| int64 majorType

                else
                    D.map CborInt32 <| apply int a

            else if majorType == 1 then
                -- Check if 64-bit integer
                if payload == 27 then
                    D.map CborInt64 <| int64 majorType

                else
                    D.map CborInt32 <| apply int a

            else if majorType == 2 then
                D.map CborBytes <| apply bytes a

            else if majorType == 3 then
                D.map CborString <| apply string a

            else if majorType == 4 then
                D.map CborList <| apply (list any) a

            else if majorType == 5 then
                D.map CborMap <| apply (associativeList any any) a

            else if majorType == 6 then
                D.map2 CborTag (apply tag a) (runDecoder any)

            else if payload == 20 then
                D.succeed <| CborBool False

            else if payload == 21 then
                D.succeed <| CborBool True

            else if payload == 22 then
                D.succeed <| CborNull

            else if payload == 23 then
                D.succeed <| CborUndefined

            else if List.member payload [ 25, 26, 27 ] then
                D.map CborFloat <| apply float a

            else
                D.fail


{-| Decode the next cbor item as a raw sequence of [`Bytes`](https://package.elm-lang.org/packages/elm/bytes/latest/Bytes#Bytes).
Note that this primitive is innefficient since it needs to parse the underlying
CBOR first, and re-encode it into a sequence afterwards. Use for debugging
purpose only.
-}
raw : Decoder Bytes
raw =
    map (CE.any >> CE.encode) any



{-------------------------------------------------------------------------------
                                  Internal
-------------------------------------------------------------------------------}


{-| Extract an element from a list
-}
extract : k -> List ( k, v ) -> Maybe ( v, List ( k, v ) )
extract needle =
    let
        go rest xs =
            case xs of
                [] ->
                    Nothing

                ( k, v ) :: tail ->
                    if needle == k then
                        Just ( v, rest ++ tail )

                    else
                        go (( k, v ) :: rest) tail
    in
    go []


{-| Marks the beginning of an indefinite structure
-}
tBEGIN : Int
tBEGIN =
    0x1F


{-| Marks the end of an indefinite structure
-}
tBREAK : Int
tBREAK =
    0xFF


runDecoder : Decoder a -> D.Decoder a
runDecoder (Decoder consumeNext processNext) =
    consumeNext |> D.andThen processNext


consumeNextMajor : Int -> (Byte -> D.Decoder a) -> Decoder a
consumeNextMajor majorType processNext =
    Decoder
        D.unsignedInt8
        (payloadForMajor majorType >> D.andThen processNext)


{-| Decode a major type and return the additional data if it matches. Major
types are encoded using 3 bits in a single byte. The meaning given to the
additional value depends on the major type itself.

           Major type -----*                  *---------- 5-bit additional data
                           |                  |
                           |                  |
                    /------------\ /----------------------\
                     2⁷ | 2⁶ | 2⁵ | 2⁴ | 2³ | 2² | 2¹ | 2⁰

-}
payloadForMajor : Int -> Byte -> D.Decoder Int
payloadForMajor majorType byte =
    if shiftRightBy 5 byte == majorType then
        D.succeed (and byte 31)

    else
        D.fail


{-| Intermediate decoder for decoding an unsigned integer. The parameter
represent the 5-bit additional information that goes with the major type and is
either the integer itself (for additional information values 0 through 23) or
the length of additional data. Additional information 24 means the value is
represented in an additional uint8\_t, 25 means a uint16\_t, 26 means a uint32\_t,
and 27 means a uint64\_t.
-}
unsigned : Int -> D.Decoder Int
unsigned a =
    if a < 24 then
        D.succeed a

    else if a == 24 then
        D.unsignedInt8

    else if a == 25 then
        D.unsignedInt16 BE

    else if a == 26 then
        D.unsignedInt32 BE

    else if a == 27 then
        D.map2 (+) (unsignedInt53 BE) (D.unsignedInt32 BE)

    else
        D.fail


{-| Decode int64 integers values as bytes (BE). This is useful in particular when
values can get in the range of 2^53 -> 2^64 - 1 (resp. -2^64 -> -2^53) which
aren't safe integer values in Elm
-}
unsignedBytes : Int -> D.Decoder Bytes
unsignedBytes a =
    if a < 24 then
        D.succeed (E.encode (E.unsignedInt8 a))

    else if a == 24 then
        D.bytes 1

    else if a == 25 then
        D.bytes 2

    else if a == 26 then
        D.bytes 4

    else if a == 27 then
        D.bytes 8

    else
        D.fail


{-| Int in Elm and JavaScript are safe in the range: -2^53 to 2^53 - 1, though,
bitwise operation works only fine for 32-bit int. As a consequence, there's no
bytes decoder for unsignedInt64 as we would need, and we've defined a custom one
herebelow that makes sure that int are decoded in an acceptable range for elm.
The parser will fail for values >= 2^53
-}
unsignedInt53 : Endianness -> D.Decoder Int
unsignedInt53 e =
    D.unsignedInt32 e
        |> D.andThen
            (\up ->
                if up > 0x001FFFFF then
                    D.fail

                else
                    D.succeed (up * 0x0000000100000000)
            )


type alias Byte =
    Int
