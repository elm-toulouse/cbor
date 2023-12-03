module Cbor.Decode exposing
    ( Decoder, decode, maybe
    , bool, int, float, string, bytes
    , list, length, dict, size
    , Step, record, fields, field, optionalField, tuple, elems, elem, optionalElem
    , succeed, fail, andThen, ignoreThen, thenIgnore, map, map2, map3, map4, map5, traverse
    , oneOf
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

@docs bool, int, float, string, bytes


## Data Structures

@docs list, length, dict, size


## Records & Tuples

@docs Step, record, fields, field, optionalField, tuple, elems, elem, optionalElem


## Mapping

@docs succeed, fail, andThen, ignoreThen, thenIgnore, map, map2, map3, map4, map5, traverse


## Branching

@docs oneOf


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
import Cbor exposing (CborItem(..))
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
    = Step
        { size : Size
        , steps : result
        , decodeKey : Decoder k
        , k : Maybe k
        }


type Size
    = Definite Int
    | Indefinite Bool


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
record decodeKey steps decodeRecord =
    consumeNextMajor 5
        (\tFirst ->
            -- Here follows an indefinite structure.
            if tFirst == tBEGIN then
                Step
                    { k = Nothing
                    , steps = steps
                    , size = Indefinite False
                    , decodeKey = decodeKey
                    }
                    |> decodeRecord
                    |> runDecoder

            else
                -- Here follows a definite structure; before we continue
                -- decoding it, we must first decode the rest of the size which
                -- may be encoded over multiple bytes.
                unsigned tFirst
                    |> D.andThen
                        (\sz ->
                            Step
                                { k = Nothing
                                , steps = steps
                                , size = Definite sz
                                , decodeKey = decodeKey
                                }
                                |> decodeRecord
                                |> runDecoder
                        )
        )
        |> map (\(Step st) -> st.steps)


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
field want v =
    andThen <|
        \(Step st) ->
            let
                decodeField got =
                    if want /= got then
                        fail

                    else
                        map (step (Step st) Nothing) v

                decodeKey =
                    case st.k of
                        Nothing ->
                            st.decodeKey

                        Just got ->
                            succeed got
            in
            decodeKey |> andThen decodeField


{-| Decode an optional field of record and step through the decoder. This
ensures that the decoded key matches the expected key. If it doesn't, the entire
decoder is failed. When the key is missing, returns `Nothing` as a value.

See [`record`](#record) for detail about usage.

-}
optionalField : k -> Decoder field -> Decoder (Step k (Maybe field -> steps)) -> Decoder (Step k steps)
optionalField want v =
    -- Optional fields are ... complicated. There are three issues:
    --
    -- 1. We need to the decode the next key, but it might not be
    --    the key we are waiting for. In such case, we mustn't fail
    --    but simply continue to the next field and pass in the key
    --    that we have already decoded.
    --
    -- 2. In the case of indefinite records, the next element might
    --    actually not be a field, but might be a break marker. If
    --    that's the case then any remaining optional field is just
    --    'Nothing' and we must not decode any new byte. We use the
    --    boolean in `Indefinite` to remember that.
    --
    -- 3. In the case of definite records, we mustn't attempt to decode
    --    the next byte because there might not be any! Which is why we
    --    keep track of the number of fields that still need to be
    --    decoded. When this is 0, we know we have decoded all fields
    --    and any new optional field is `Nothing`.
    andThen <|
        \(Step st) ->
            let
                decodeField got =
                    if want /= got then
                        step (Step st) (Just got) Nothing |> succeed

                    else
                        v |> map (Just >> step (Step st) Nothing)

                ignoreField =
                    step (Step st) st.k Nothing
            in
            case st.size of
                Indefinite done ->
                    case st.k of
                        Nothing ->
                            if done then
                                succeed ignoreField

                            else
                                let
                                    (Decoder consumeKey processKey) =
                                        st.decodeKey
                                in
                                Decoder consumeKey <|
                                    \t ->
                                        if t == tBREAK then
                                            let
                                                (Step stNext) =
                                                    ignoreField
                                            in
                                            D.succeed <| Step { stNext | size = Indefinite True }

                                        else
                                            processKey t |> D.andThen (decodeField >> runDecoder)

                        Just got ->
                            decodeField got

                Definite sz ->
                    let
                        decodeKey =
                            case st.k of
                                Nothing ->
                                    st.decodeKey

                                Just got ->
                                    succeed got
                    in
                    if sz <= 0 then
                        succeed ignoreField

                    else
                        decodeKey |> andThen decodeField


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
                Step
                    { k = Nothing
                    , steps = steps
                    , size = Indefinite False
                    , decodeKey = fail
                    }
                    |> decodeTuple
                    |> andThen
                        (\(Step st) ->
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
                            Step
                                { k = Nothing
                                , steps = steps
                                , size = Definite sz
                                , decodeKey = fail
                                }
                                |> decodeTuple
                                |> runDecoder
                        )
                    |> D.map (\(Step st) -> st.steps)


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
    andThen <|
        \(Step st) -> map (step (Step st) Nothing) v


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
    andThen <|
        \(Step st) ->
            let
                ignoreElem =
                    step (Step st) st.k Nothing
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
                                    let
                                        (Step sNext) =
                                            ignoreElem
                                    in
                                    D.succeed <| Step { sNext | size = Indefinite True }

                                else
                                    D.map (step (Step st) Nothing << Just) (processField t)

                Definite sz ->
                    if sz <= 0 then
                        succeed ignoreElem

                    else
                        map (step (Step st) Nothing << Just) v


{-| Internal, generic stepping function
-}
step : Step k (field -> steps) -> Maybe k -> field -> Step k steps
step (Step st) k next =
    Step
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
    Decoder (D.succeed absurd) <|
        (alternatives
            |> List.map runDecoder
            |> D.oneOf
            |> always
        )



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
                D.map CborInt <| apply int a

            else if majorType == 1 then
                D.map CborInt <| apply int a

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
