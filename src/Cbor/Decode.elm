module Cbor.Decode exposing
    ( Decoder, decode
    , bool, int, float, string, bytes
    , list, array, dict, keyValueMap, record, pair, maybe
    , succeed, fail, andThen, map, map2, map3, map4, map5
    , tag, tagged
    , any
    )

{-| The Concise Binary Object Representation (CBOR) is a data format whose design
goals include the possibility of extremely small code size, fairly small message
size, and extensibility without the need for version negotiation. These design
goals make it different from earlier binary serializations such as ASN.1 and
MessagePack.


## Decoder

@docs Decoder, decode


## Primitives

@docs bool, int, float, string, bytes


## Data Structures

@docs list, array, dict, keyValueMap, record, pair, maybe


## Mapping

@docs succeed, fail, andThen, map, map2, map3, map4, map5


## Tagging

@docs tag, tagged


## Debugging

@docs any

-}

import Bitwise exposing (and, or, shiftLeftBy, shiftRightBy)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D
import Bytes.Encode as E
import Bytes.Floating.Decode as D
import Cbor exposing (CborItem(..))
import Cbor.Tag exposing (Tag(..))
import Dict exposing (Dict)
import Tuple exposing (first)



{------------------------------------------------------------------------------
                                  Decoder
------------------------------------------------------------------------------}


{-| Describes how to turn a binary CBOR sequence of bytes into a nice Elm value.
-}
type Decoder a
    = Decoder MajorType (Int -> D.Decoder a)


{-| Turn a binary CBOR sequence of bytes into a nice Elm value.

    import Cbor.Decode as D
    import Url exposing (Url)

    type alias Album =
        { artist : String
        , title : String
        , year : Int
        , tracks : List ( String, Duration )
        , links : List Url
        }

    type Duration
        = Duration Int

    decodeAlbum : D.Decoder Album
    decodeAlbum =
        let
            link =
                D.string
                    |> D.map Url.fromString
                    |> D.andThen (Maybe.map D.succeed >> Maybe.withDefault D.fail)

            track =
                D.pair D.string (D.map Duration D.int)
        in
        D.map5 Album
            D.string
            D.string
            D.int
            (D.list track)
            (D.list link)

-}
decode : Decoder a -> Bytes -> Maybe a
decode d =
    D.decode (runDecoder d)



{-------------------------------------------------------------------------------
                                 Primitives
-------------------------------------------------------------------------------}


{-| Decode a boolean.
-}
bool : Decoder Bool
bool =
    Decoder (MajorType 7) <|
        \a ->
            if a == 20 then
                D.succeed False

            else if a == 21 then
                D.succeed True

            else
                D.fail


{-| Decode an integer. Note that there's no granular decoders (nor encoders) for
integers like 'unsignedInt8' or 'signedInt32' because, the CBOR encoding
of an integers varies depending on the integer value. Therefore, at the
CBOR-level, as in Elm, there's no notion of _smaller_ integers.
-}
int : Decoder Int
int =
    -- NOTE Unfortunately, we don't have any 'Alternative'-ish instance on
    -- @Byte.Decoder@, or something like 'oneOf' to try several decoders in
    -- sequence. Since Elm conflates representation of unsigned and negative
    -- integer into one 'int' type, we have to define an ad-hoc decoder for
    -- the major types here to handle both the Major type 0 and 1.
    Decoder MajorTypeInPayload
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

    E.Encode (E.float16 1.1) == Bytes<0xF9, 0x3C, 0x66>

    E.Encode (E.float64 1.1) == Bytes<0xFB, 0x3F, 0xF1, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9A>

-}
float : Decoder Float
float =
    Decoder (MajorType 7) <|
        \a ->
            if a == 25 then
                D.float16 BE

            else if a == 26 then
                D.float32 BE

            else if a == 27 then
                D.float64 BE

            else
                D.fail


{-| Decode a bunch UTF-8 bytes into a 'String'. In case of streaming, all
strings are decoded at once and concatenated as if they were one big string.
See also 'Cbor.Encode.beginStrings'
-}
string : Decoder String
string =
    let
        indef es =
            D.unsignedInt8
                |> D.andThen
                    (\a ->
                        if a == tBREAK then
                            es
                                |> List.reverse
                                |> String.concat
                                |> D.Done
                                |> D.succeed

                        else
                            withMajorType 3 a
                                |> D.andThen unsigned
                                |> D.andThen D.string
                                |> D.map (\e -> D.Loop (e :: es))
                    )
    in
    Decoder (MajorType 3) <|
        \a ->
            if a == tBEGIN then
                D.loop [] indef

            else
                unsigned a |> D.andThen D.string


{-| Decode a bunch bytes into 'Bytes'. In case of streaming, all
bytes are decoded at once and concatenated as if they were one big byte string.
See also 'Cbor.Encode.beginBytes'
-}
bytes : Decoder Bytes
bytes =
    let
        indef es =
            D.unsignedInt8
                |> D.andThen
                    (\a ->
                        if a == tBREAK then
                            es
                                |> List.reverse
                                |> List.map E.bytes
                                |> E.sequence
                                |> E.encode
                                |> D.Done
                                |> D.succeed

                        else
                            withMajorType 2 a
                                |> D.andThen unsigned
                                |> D.andThen D.bytes
                                |> D.map (\e -> D.Loop (e :: es))
                    )
    in
    Decoder (MajorType 2) <|
        \a ->
            if a == tBEGIN then
                D.loop [] indef

            else
                unsigned a |> D.andThen D.bytes



{-------------------------------------------------------------------------------
                              Data-Structures
-------------------------------------------------------------------------------}


{-| Decode a 'List' of items 'a'. The list can be finite or infinite (see also
_Streaming_ in Cbor.Encode).

    D.decode (D.list D.int) Bytes<0x82, 0x0E, 0x18, 0x2A> == Just [ 14, 42 ]

    D.decode (D.list D.int) Bytes<0x9F, 0x01, 0x02, 0xFF> == Just [ 1, 1 ]

    D.decode (D.list (D.list D.bool)) Bytes<0x81, 0x9F, 0xF4, 0xFF> == Just [ [ False ] ]

-}
list : Decoder a -> Decoder (List a)
list ((Decoder major payload) as elem) =
    let
        finite ( n, es ) =
            if n <= 0 then
                es |> List.reverse |> D.Done |> D.succeed

            else
                runDecoder elem |> D.map (\e -> D.Loop ( n - 1, e :: es ))

        indef es =
            D.unsignedInt8
                |> D.andThen
                    (\a ->
                        if a == tBREAK then
                            es |> List.reverse |> D.Done |> D.succeed

                        else
                            continueDecoder a elem
                                |> D.map (\e -> D.Loop (e :: es))
                    )
    in
    Decoder (MajorType 4) <|
        \a ->
            if a == tBEGIN then
                D.loop [] indef

            else
                unsigned a |> D.andThen (\n -> D.loop ( n, [] ) finite)


{-| Decode an array or said differently, a list with heterogeneous elements
(with different types).

Since Elm (or statically typed languages in general) does not support hetegorenous arrays, we
have to resort to records when parsing. This parser works best with the `map`
family of functions.

For example, to decode an array with 3 elements:

    D.decode (D.array <| D.map3 MyRecord D.bool D.int D.string) (Bytes<0x83 0xF5 0x0E 0x61 0x61>)
        == Just (MyRecord True 14 "a")

-}
array : Decoder a -> Decoder a
array =
    Decoder (MajorType 4) << always << runDecoder


{-| Decode a 2-tuple. This is mostly a helper around a list decoder with 2
elements, with non-uniform types.

    D.decode (D.pair D.int D.bool) Bytes<0x0E, 0xF5> == Just ( 14, True )

-}
pair : Decoder a -> Decoder b -> Decoder ( a, b )
pair a b =
    map2 Tuple.pair a b


{-| Decode an CBOR map of pairs of data-items into an Elm 'Dict'. The map can be
either of finite length or indefinite length (see also _Streaming_ in Cbor.Encode).

    D.decode (D.dict D.int D.int) Bytes<0xA1, 0x0E, 0x18, 0x2A>
        == Dict.fromList [ ( 14, 42 ) ]

    D.decode (D.dict D.string D.int) Bytes<0xBF, 0x61, 0x61, 0x0E, 0x61, 0x62, 0x18, 0x2A, 0xFF>
        == Dict.fromList [ ( "a", 14 ), ( "b", 42 ) ]

-}
dict : Decoder comparable -> Decoder a -> Decoder (Dict comparable a)
dict key value =
    map Dict.fromList <| keyValueMap key value


{-| Decode a CBOR (key, value) map, where keys can be potentially arbitrary CBOR.

If keys are `comparable`, you should prefer `dict` to this primitive.

-}
keyValueMap : Decoder k -> Decoder v -> Decoder (List ( k, v ))
keyValueMap key value =
    let
        finite ( n, es ) =
            if n <= 0 then
                es |> List.reverse |> D.Done |> D.succeed

            else
                D.map2 Tuple.pair (runDecoder key) (runDecoder value)
                    |> D.map (\e -> D.Loop ( n - 1, e :: es ))

        indef es =
            D.unsignedInt8
                |> D.andThen
                    (\a ->
                        if a == tBREAK then
                            es |> List.reverse |> D.Done |> D.succeed

                        else
                            D.map2 Tuple.pair (continueDecoder a key) (runDecoder value)
                                |> D.map (\e -> D.Loop (e :: es))
                    )
    in
    Decoder (MajorType 5) <|
        \a ->
            if a == tBEGIN then
                D.loop [] indef

            else
                unsigned a |> D.andThen (\n -> D.loop ( n, [] ) finite)


{-| Like 'array', but for heterogeneous dict.
-}
record : Decoder a -> Decoder a
record =
    Decoder (MajorType 5) << always << runDecoder


{-| Helpful for dealing with optional fields. Turns `null` into `Nothing`.

    D.decode (D.maybe D.bool) Bytes<0xF6> == Just Nothing

    D.decode (D.maybe D.bool) Bytes<0xF4> == Just (Just False)

-}
maybe : Decoder a -> Decoder (Maybe a)
maybe ((Decoder major payload) as decoder) =
    let
        runMaybe a =
            if a == 0xF6 then
                D.succeed Nothing

            else
                D.map Just (continueDecoder a decoder)
    in
    Decoder MajorTypePlaceholder <|
        \x ->
            case major of
                MajorTypePlaceholder ->
                    D.map Just (payload x)

                _ ->
                    if x == tPLACEHOLDER then
                        D.unsignedInt8 |> D.andThen runMaybe

                    else
                        runMaybe x



{-------------------------------------------------------------------------------
                                  Tagging
-------------------------------------------------------------------------------}


{-| This implementation does little interpretation of the tags and is limited to
only decoding the tag's value. The tag's payload has to be specified as any
other decoder. So, using the previous example, one could decode a bignum as:

    D.decode (D.tag |> D.andThen (\tag -> D.bytes)) input

You may also use @tagged@ if you as a helper to decode a tagged value, while
verifying that the tag matches what you expect.

    D.decode (D.tagged PositiveBigNum D.bytes) input

-}
tag : Decoder Tag
tag =
    Decoder (MajorType 6) <|
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


{-| Decode a value that is tagged with the given 'Tag'. Fails if the value is
not tagged, or, tag with some other 'Tag'

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

This particularly handy when used in combination with `andThen`.

-}
succeed : a -> Decoder a
succeed a =
    Decoder MajorTypePlaceholder (\_ -> D.succeed a)


{-| A decoder that always fail.

    D.decode D.fail Bytes<0x00> == Nothing

This is particularly handy when used in combination with `andThen`.

-}
fail : Decoder a
fail =
    Decoder MajorTypePlaceholder (\_ -> D.fail)


{-| Decode something and then use that information to decode something else.
This is useful when a 'Decoder' depends on a value held by another decoder:

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
andThen fn a =
    Decoder MajorTypePlaceholder (\x -> runOrContinue x a |> D.andThen (fn >> runDecoder))


{-| Transform a decoder.

For example, maybe you just want to know the length of a string:

    import String

    stringLength : Decoder Int
    stringLength =
        D.map String.length D.string

-}
map : (a -> value) -> Decoder a -> Decoder value
map fn a =
    Decoder MajorTypePlaceholder (\x -> runOrContinue x a |> D.map fn)


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
map2 fn a b =
    Decoder MajorTypePlaceholder <|
        \x ->
            D.map2 fn
                (runOrContinue x a)
                (runDecoder b)


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
map3 fn a b c =
    Decoder MajorTypePlaceholder <|
        \x ->
            D.map3 fn
                (runOrContinue x a)
                (runDecoder b)
                (runDecoder c)


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
map4 fn a b c d =
    Decoder MajorTypePlaceholder <|
        \x ->
            D.map4 fn
                (runOrContinue x a)
                (runDecoder b)
                (runDecoder c)
                (runDecoder d)


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
map5 fn a b c d e =
    Decoder MajorTypePlaceholder <|
        \x ->
            D.map5 fn
                (runOrContinue x a)
                (runDecoder b)
                (runDecoder c)
                (runDecoder d)
                (runDecoder e)



{-------------------------------------------------------------------------------
                                  Debugging
-------------------------------------------------------------------------------}


{-| Decode remaining bytes as _any_ `CborItem`. This is useful for debugging
or to inspect some unknown Cbor data.

    D.decode D.any <| E.encode (E.int 14) == Just (CborUnsignedInteger 14)

-}
any : Decoder CborItem
any =
    Decoder MajorTypeInPayload <|
        \a ->
            let
                majorType =
                    shiftRightBy 5 a

                payload =
                    and a 31

                apply : Decoder a -> Int -> D.Decoder a
                apply (Decoder _ decoder) i =
                    decoder i
            in
            if majorType == 0 then
                D.map CborInt <| apply int a

            else if majorType == 1 then
                D.map CborInt <| apply int a

            else if majorType == 2 then
                D.map CborBytes <| apply bytes payload

            else if majorType == 3 then
                D.map CborString <| apply string payload

            else if majorType == 4 then
                D.map CborList <| apply (list any) payload

            else if majorType == 5 then
                D.map CborMap <| apply (keyValueMap any any) payload

            else if majorType == 6 then
                D.map CborTag <| apply tag payload

            else if payload == 20 then
                D.succeed <| CborBool False

            else if payload == 21 then
                D.succeed <| CborBool True

            else if payload == 22 then
                D.succeed <| CborNull

            else if payload == 23 then
                D.succeed <| CborUndefined

            else if List.member payload [ 25, 26, 27 ] then
                D.map CborFloat <| apply float payload

            else
                D.fail



{-------------------------------------------------------------------------------
                                  Internal
-------------------------------------------------------------------------------}


{-| Marks the beginning of an indefinite structure
-}
tBEGIN : Int
tBEGIN =
    31


{-| Marks the end of an indefinite structure
-}
tBREAK : Int
tBREAK =
    0xFF


{-| Marks a top-level combinator which no major type. 30 is an unassigned value
for major types (nor a special case like 31 = 'break'). So, we use it as a
special marker to indicate that a special decoder is in a top-level position.
-}
tPLACEHOLDER : Int
tPLACEHOLDER =
    30


{-| Run a decoder in sequence, getting first the major type, and then the
payload. Note that, separating the definition of the payload and the major type
allows us to implement various things like indefinite list or maybes, where, we
can start to peak at the next byte and take action depending on its value.
This is only possible because all items are prefixed with a major type
announcing what they are!
-}
runDecoder : Decoder a -> D.Decoder a
runDecoder (Decoder major payload) =
    case major of
        MajorTypePlaceholder ->
            payload tPLACEHOLDER

        MajorTypeInPayload ->
            D.unsignedInt8 |> D.andThen payload

        MajorType m ->
            D.unsignedInt8 |> D.andThen (withMajorType m) |> D.andThen payload


{-| Continue a decoder with the given 'Int' token parsed outside of the decoder.
This happens for indefinite data-structure where we have to first peak at the
next token before knowing which parser hsa to be ran (if any).
-}
continueDecoder : Int -> Decoder a -> D.Decoder a
continueDecoder a (Decoder major payload) =
    case major of
        MajorTypePlaceholder ->
            payload a

        MajorTypeInPayload ->
            payload a

        MajorType m ->
            withMajorType m a |> D.andThen payload


{-| In some cases (like 'andThen' or 'map'), we don't have enough context to
figure out whether we need to consume the next major type token or not. When a
decoder has already been ran and has yield a value /= tPLACEHOLDER, we can just
continue decoding with that particular token. Otherwise, we haven't yet consumed
a token for the major type and we should simply run the actual decoder instead
of continuing it.
-}
runOrContinue : Int -> Decoder a -> D.Decoder a
runOrContinue a d =
    if a == tPLACEHOLDER then
        runDecoder d

    else
        continueDecoder a d


{-| Decode a major type and return the additional data if it matches. Major
types are encoded using 3 bits in a single byte. The meaning given to the
additional value depends on the major type itself.

           Major type -----*                  *---------- 5-bit additional data
                           |                  |
                           |                  |
                    /------------\ /----------------------\
                     2⁷ | 2⁶ | 2⁵ | 2⁴ | 2³ | 2² | 2¹ | 2⁰

-}
withMajorType : Int -> Int -> D.Decoder Int
withMajorType k a =
    if shiftRightBy 5 a == k then
        D.succeed (and a 31)

    else
        D.fail


{-| Internal representation of Major type
-}
type MajorType
    = MajorType Int
    | MajorTypeInPayload
    | MajorTypePlaceholder


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
