module Cbor.Decode exposing
    ( Decoder, decodeBytes
    , bool, int, float, string, bytes
    , list, dict, pair, maybe
    , succeed, fail, andThen, map, map2, map3, map4, map5
    , Tag(..), tag, tagged
    )

{-| The Concise Binary Object Representation (CBOR) is a data format whose design
goals include the possibility of extremely small code size, fairly small message
size, and extensibility without the need for version negotiation. These design
goals make it different from earlier binary serializations such as ASN.1 and
MessagePack.


## Decoder

@docs Decoder, decodeBytes


## Primitives

@docs bool, int, float, string, bytes


## Data Structures

@docs list, dict, pair, maybe


## Mapping

@docs succeed, fail, andThen, map, map2, map3, map4, map5


## Tagging

@docs Tag, tag, tagged

-}

import Bitwise exposing (and, or, shiftLeftBy, shiftRightBy)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D
import Bytes.Encode as E
import Bytes.Floating.Decode as D
import Dict exposing (Dict)
import Tuple exposing (first)



{------------------------------------------------------------------------------
                                  Decoder
------------------------------------------------------------------------------}


type Decoder a
    = Decoder MajorType (Int -> D.Decoder a)


decodeBytes : Decoder a -> Bytes -> Maybe a
decodeBytes d =
    D.decode (runDecoder d)



{-------------------------------------------------------------------------------
                                 Primitives
-------------------------------------------------------------------------------}


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
                            majorType 3 a
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
                            majorType 2 a
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


pair : Decoder a -> Decoder b -> Decoder ( a, b )
pair a b =
    Decoder (MajorType 4) <|
        unsigned
            >> D.andThen
                (\n ->
                    if n /= 2 then
                        D.fail

                    else
                        D.map2 Tuple.pair (runDecoder a) (runDecoder b)
                )


dict : Decoder comparable -> Decoder a -> Decoder (Dict comparable a)
dict key value =
    let
        finite ( n, es ) =
            if n <= 0 then
                es |> List.reverse |> Dict.fromList |> D.Done |> D.succeed

            else
                D.map2 Tuple.pair (runDecoder key) (runDecoder value)
                    |> D.map (\e -> D.Loop ( n - 1, e :: es ))

        indef es =
            D.unsignedInt8
                |> D.andThen
                    (\a ->
                        if a == tBREAK then
                            es |> List.reverse |> Dict.fromList |> D.Done |> D.succeed

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
                                  Mapping
-------------------------------------------------------------------------------}


succeed : a -> Decoder a
succeed a =
    Decoder MajorTypePlaceholder (\_ -> D.succeed a)


fail : Decoder a
fail =
    Decoder MajorTypePlaceholder (\_ -> D.fail)


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen fn a =
    Decoder MajorTypePlaceholder (\x -> runOrContinue x a |> D.andThen (fn >> runDecoder))


map : (a -> value) -> Decoder a -> Decoder value
map fn a =
    Decoder MajorTypePlaceholder (\x -> runOrContinue x a |> D.map fn)


map2 : (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
map2 fn a b =
    Decoder MajorTypePlaceholder <|
        \x ->
            D.map2 fn
                (runOrContinue x a)
                (runDecoder b)


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
                                  Tagging
-------------------------------------------------------------------------------}


{-| Optional semantic tags as specified in the RFC 7049. Tags can be used to
give an extra meaning to a generic piece of data that follows it. For instance,
one could encode a bignum as a raw byte string and add a corresponding tag 0x02
to indicates what meaning can be given to that bytestring.

This implementation does little interpretation of the tags and is limited to
only decoding the tag's value. The tag's payload has to be specified as any
other decoder. So, using the previous example, one could decode a bignum as:

    >>> decodeBytes (tag |> andThen (\t -> bytes)) input

You may also use @tagged@ if you as a helper to decode a tagged value, while
verifying that the tag matches what you expect.

    >>> decodeBytes (tagged PositiveBigNum bytes) input

-}
type Tag
    = StandardDateTime
    | EpochDateTime
    | PositiveBigNum
    | NegativeBigNum
    | DecimalFraction
    | BigFloat
    | Base64UrlConversion
    | Base64Conversion
    | Base16Conversion
    | Cbor
    | Uri
    | Base64Url
    | Base64
    | Regex
    | Mime
    | IsCbor
    | Unknown Int


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
            D.unsignedInt8 |> D.andThen (majorType m) |> D.andThen payload


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
            majorType m a |> D.andThen payload


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
majorType : Int -> Int -> D.Decoder Int
majorType k a =
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
