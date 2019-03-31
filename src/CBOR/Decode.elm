module CBOR.Decode exposing
    ( Decoder(..), decodeBytes
    , bool, int, float, string, bytes
    , list, dict
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

@docs list, dict


## Mapping

@docs succeed, fail, andThen, map, map2, map3, map4, map5


## Tagging

@docs Tag, tag, tagged

-}

import Bitwise exposing (and, or, shiftLeftBy, shiftRightBy)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Bytes
import Bytes.Encode
import Dict exposing (Dict)
import Tuple exposing (first)



{------------------------------------------------------------------------------
                                  Decoder
------------------------------------------------------------------------------}


type Decoder a
    = Decoder (Bytes.Decoder Int) (Int -> Bytes.Decoder a)


decodeBytes : Decoder a -> Bytes -> Maybe a
decodeBytes d =
    Bytes.decode (runDecoder d)



{-------------------------------------------------------------------------------
                                 Primitives
-------------------------------------------------------------------------------}


bool : Decoder Bool
bool =
    Decoder (majorType 7) <|
        \a ->
            if a == 20 then
                Bytes.succeed False

            else if a == 21 then
                Bytes.succeed True

            else
                Bytes.fail


int : Decoder Int
int =
    -- NOTE Unfortunately, we don't have any 'Alternative'-ish instance on
    -- @Byte.Decoder@, or something like 'oneOf' to try several decoders in
    -- sequence. Since Elm conflates representation of unsigned and negative
    -- integer into one 'int' type, we have to define an ad-hoc decoder for
    -- the major types here to handle both the Major type 0 and 1.
    Decoder Bytes.unsignedInt8 <|
        \a ->
            if shiftRightBy 5 a == 0 then
                unsigned a

            else if shiftRightBy 5 a == 1 then
                Bytes.map (\x -> negate x - 1) (unsigned (and a 31))

            else
                Bytes.fail


float : Decoder Float
float =
    Decoder (majorType 7) <|
        \a ->
            if a == 25 then
                float16

            else if a == 26 then
                Bytes.float32 BE

            else if a == 27 then
                Bytes.float64 BE

            else
                Bytes.fail


string : Decoder String
string =
    let
        indef es =
            Bytes.unsignedInt8
                |> Bytes.andThen
                    (\a ->
                        if a == 0xFF then
                            es
                                |> List.reverse
                                |> String.concat
                                |> Bytes.Done
                                |> Bytes.succeed

                        else
                            majorTypeRaw 3 a
                                |> Bytes.andThen unsigned
                                |> Bytes.andThen Bytes.string
                                |> Bytes.map (\e -> Bytes.Loop (e :: es))
                    )
    in
    Decoder (majorType 3) <|
        \a ->
            if a == 31 then
                Bytes.loop [] indef

            else
                unsigned a |> Bytes.andThen Bytes.string


bytes : Decoder Bytes
bytes =
    let
        indef es =
            Bytes.unsignedInt8
                |> Bytes.andThen
                    (\a ->
                        if a == 0xFF then
                            es
                                |> List.reverse
                                |> List.map Bytes.Encode.bytes
                                |> Bytes.Encode.sequence
                                |> Bytes.Encode.encode
                                |> Bytes.Done
                                |> Bytes.succeed

                        else
                            majorTypeRaw 2 a
                                |> Bytes.andThen unsigned
                                |> Bytes.andThen Bytes.bytes
                                |> Bytes.map (\e -> Bytes.Loop (e :: es))
                    )
    in
    Decoder (majorType 2) <|
        \a ->
            if a == 31 then
                Bytes.loop [] indef

            else
                unsigned a |> Bytes.andThen Bytes.bytes



{-------------------------------------------------------------------------------
                              Data-Structures
-------------------------------------------------------------------------------}


list : Decoder a -> Decoder (List a)
list ((Decoder _ elemPayload) as elem) =
    let
        finite ( n, es ) =
            if n <= 0 then
                es |> List.reverse |> Bytes.Done |> Bytes.succeed

            else
                runDecoder elem |> Bytes.map (\e -> Bytes.Loop ( n - 1, e :: es ))

        indef es =
            Bytes.unsignedInt8
                |> Bytes.andThen
                    (\a ->
                        if a == 0xFF then
                            es |> List.reverse |> Bytes.Done |> Bytes.succeed

                        else
                            -- TODO Be more strict on the elem payload validation
                            -- Ideally, we would verify that the major type
                            -- matches whatever was expected from the original
                            -- decoder. This could be done via defining the
                            -- major type as '(Int -> Bytes.Decoder a)' in the
                            -- 'Decoder'
                            elemPayload (and a 31) |> Bytes.map (\e -> Bytes.Loop (e :: es))
                    )
    in
    Decoder (majorType 4) <|
        \a ->
            if a == 31 then
                Bytes.loop [] indef

            else
                unsigned a |> Bytes.andThen (\n -> Bytes.loop ( n, [] ) finite)


dict : Decoder comparable -> Decoder a -> Decoder (Dict comparable a)
dict ((Decoder _ keyPayload) as key) value =
    let
        finite ( n, es ) =
            if n <= 0 then
                es |> List.reverse |> Dict.fromList |> Bytes.Done |> Bytes.succeed

            else
                Bytes.map2 Tuple.pair (runDecoder key) (runDecoder value)
                    |> Bytes.map (\e -> Bytes.Loop ( n - 1, e :: es ))

        indef es =
            Bytes.unsignedInt8
                |> Bytes.andThen
                    (\a ->
                        if a == 0xFF then
                            es |> List.reverse |> Dict.fromList |> Bytes.Done |> Bytes.succeed

                        else
                            -- TODO Be more strict on the key payload validation
                            -- Ideally, we would verify that the major type
                            -- matches whatever was expected from the original
                            -- decoder. This could be done via defining the
                            -- major type as '(Int -> Bytes.Decoder a)' in the
                            -- 'Decoder'
                            Bytes.map2 Tuple.pair (keyPayload (and a 31)) (runDecoder value)
                                |> Bytes.map (\e -> Bytes.Loop (e :: es))
                    )
    in
    Decoder (majorType 5) <|
        \a ->
            if a == 31 then
                Bytes.loop [] indef

            else
                unsigned a |> Bytes.andThen (\n -> Bytes.loop ( n, [] ) finite)



{-------------------------------------------------------------------------------
                                  Mapping
-------------------------------------------------------------------------------}


succeed : a -> Decoder a
succeed a =
    -- "major type" here can be anything BUT 31 (marker for indefinite structs)
    -- We currently use 30 because it's unassigned.
    Decoder (Bytes.succeed 0) (\_ -> Bytes.succeed a)


fail : Decoder a
fail =
    Decoder Bytes.fail (\_ -> Bytes.fail)


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen fn a =
    Decoder (Bytes.succeed 0) (\_ -> runDecoder a |> Bytes.andThen (fn >> runDecoder))


map : (a -> value) -> Decoder a -> Decoder value
map fn a =
    Decoder (Bytes.succeed 0) (\_ -> runDecoder a |> Bytes.map fn)


map2 : (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
map2 fn a b =
    Decoder (Bytes.succeed 0) <|
        \_ ->
            Bytes.map2 fn
                (runDecoder a)
                (runDecoder b)


map3 :
    (a -> b -> c -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder value
map3 fn a b c =
    Decoder (Bytes.succeed 0) <|
        \_ ->
            Bytes.map3 fn
                (runDecoder a)
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
    Decoder (Bytes.succeed 0) <|
        \_ ->
            Bytes.map4 fn
                (runDecoder a)
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
    Decoder (Bytes.succeed 0) <|
        \_ ->
            Bytes.map5 fn
                (runDecoder a)
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
    Decoder (majorType 6) <|
        unsigned
            >> Bytes.map
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


{-| Run a decoder in sequence, getting first the major type, and then the
payload. Note that, separating the definition of the payload and the major type
allows us to implement various things like indefinite list or maybes, where, we
can start to peak at the next byte and take action depending on its value.
This is only possible because all items are prefixed with a major type
announcing what they are!
-}
runDecoder : Decoder a -> Bytes.Decoder a
runDecoder (Decoder major payload) =
    major |> Bytes.andThen payload


{-| Decode a major type and return the additional data if it matches. Major
types are encoded using 3 bits in a single byte. The meaning given to the
additional value depends on the major type itself.

           Major type -----*                  *---------- 5-bit additional data
                           |                  |
                           |                  |
                    /------------\ /----------------------\
                     2⁷ | 2⁶ | 2⁵ | 2⁴ | 2³ | 2² | 2¹ | 2⁰

-}
majorType : Int -> Bytes.Decoder Int
majorType k =
    Bytes.unsignedInt8 |> Bytes.andThen (majorTypeRaw k)


{-| Continue parsing major type with the given value 'a'. It give the caller the
possiblity to do something
with the raw value of 'a' before it gets normalized
-}
majorTypeRaw : Int -> Int -> Bytes.Decoder Int
majorTypeRaw k a =
    if shiftRightBy 5 a == k then
        Bytes.succeed (and a 31)

    else
        Bytes.fail


{-| Int in Elm and JavaScript are safe in the range: -2^53 to 2^53 - 1, though,
bitwise operation works only fine for 32-bit int. As a consequence, there's no
bytes decoder for unsignedInt64 as we would need, and we've defined a custom one
herebelow that makes sure that int are decoded in an acceptable range for elm.
The parser will fail for values >= 2^53
-}
unsignedInt53 : Endianness -> Bytes.Decoder Int
unsignedInt53 e =
    Bytes.unsignedInt32 e
        |> Bytes.andThen
            (\up ->
                if up > 0x001FFFFF then
                    Bytes.fail

                else
                    Bytes.succeed (up * 0x0000000100000000)
            )


{-| Decoder of IEEE 754 Half-Precision Float (16-bit)

       exponent
              |        mantissa
    sign      |               |
       |      |               |
       |      |               |
      / \/---------\/-------------------\
       *  * * * * *  * * * * * * * * * *  (16-bit)

    ------------------|-----------------------------------------
    e in [1..30]      | h = (-1)^s * 2 ^ (e - 15) * 1.mmmmmmmmmm
    e == 0 && m /= 0  | h = (-1)^s * 2 ^ -14 * 0.mmmmmmmmmm
    e == 0 && m == 0  | h = +/- 0.0
    e == 31 && m == 0 | h = +/- Infinity
    e == 31 && m /= 0 | h = NaN

Note that since we are converting from half-precision to single precision,
there a gain in precision and some numbers may end up with more decimals in
their float 32-bit representation (for instance: 65504.0 as 0xF97BFF, ends up
as 65503.996723200005)

-}
float16 : Bytes.Decoder Float
float16 =
    Bytes.unsignedInt16 BE
        |> Bytes.map
            (\n ->
                let
                    s =
                        shiftRightBy 15 n

                    e =
                        and 31 (shiftRightBy 10 n)

                    m =
                        and 1023 n

                    mantissa k =
                        List.sum
                            [ 0.5 * (shiftRightBy 9 k |> toFloat)
                            , 0.25 * (and 256 k |> shiftRightBy 8 |> toFloat)
                            , 0.125 * (and 128 k |> shiftRightBy 7 |> toFloat)
                            , 0.0625 * (and 64 k |> shiftRightBy 6 |> toFloat)
                            , 0.03125 * (and 32 k |> shiftRightBy 5 |> toFloat)
                            , 0.015625 * (and 16 k |> shiftRightBy 4 |> toFloat)
                            , 0.0078125 * (and 8 k |> shiftRightBy 3 |> toFloat)
                            , 0.00390625 * (and 4 k |> shiftRightBy 2 |> toFloat)
                            , 0.001953125 * (and 2 k |> shiftRightBy 1 |> toFloat)
                            , 0.0009764625 * (and 1 k |> toFloat)
                            ]
                in
                if e >= 1 && e <= 30 then
                    (-1 ^ s |> toFloat) * (2 ^ (e - 15) |> toFloat) * (1.0 + mantissa m)

                else if e == 0 && m /= 0 then
                    (-1 ^ s |> toFloat) * (2 ^ -14 |> toFloat) * mantissa m

                else if e == 0 && m == 0 then
                    0.0

                else if e == 31 && m == 0 then
                    -- isInfinite (1/0) == True && isInfinite (-1/0) == True
                    (-1 ^ s |> toFloat) / 0

                else
                    -- isNaN (0/0) == True
                    0 / 0
            )


{-| Intermediate decoder for decoding an unsigned integer. The parameter
represent the 5-bit additional information that goes with the major type and is
either the integer itself (for additional information values 0 through 23) or
the length of additional data. Additional information 24 means the value is
represented in an additional uint8\_t, 25 means a uint16\_t, 26 means a uint32\_t,
and 27 means a uint64\_t.
-}
unsigned : Int -> Bytes.Decoder Int
unsigned a =
    if a < 24 then
        Bytes.succeed a

    else if a == 24 then
        Bytes.unsignedInt8

    else if a == 25 then
        Bytes.unsignedInt16 BE

    else if a == 26 then
        Bytes.unsignedInt32 BE

    else if a == 27 then
        Bytes.map2 (+) (unsignedInt53 BE) (Bytes.unsignedInt32 BE)

    else
        Bytes.fail
