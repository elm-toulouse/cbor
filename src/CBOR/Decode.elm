module CBOR.Decode exposing
    ( Decoder(..), decodeBytes
    , bool, int, float, string, bytes
    , list, dict
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

-}

import Bitwise exposing (and, or, shiftLeftBy, shiftRightBy)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Bytes
import Bytes.Encode as Encode exposing (encode, sequence)
import Dict exposing (Dict)
import Tuple exposing (first)



{------------------------------------------------------------------------------
                                  Decoder
------------------------------------------------------------------------------}


type Decoder a
    = Decoder (Bytes.Decoder a)


decodeBytes : Decoder a -> Bytes -> Maybe a
decodeBytes (Decoder decoder) =
    Bytes.decode decoder



{-------------------------------------------------------------------------------
                                 Primitives
-------------------------------------------------------------------------------}


bool : Decoder Bool
bool =
    let
        bool_ a =
            if a == 20 then
                Bytes.succeed False

            else if a == 21 then
                Bytes.succeed True

            else
                Bytes.fail
    in
    majorType 7
        |> Bytes.andThen bool_
        |> Decoder


int : Decoder Int
int =
    let
        -- NOTE Unfortunately, we don't have any 'Alternative'-ish instance on
        -- @Byte.Decoder@, or something like 'oneOf' to try several decoders in
        -- sequence. Since Elm conflates representation of unsigned and negative
        -- integer into one 'int' type, we have to define an ad-hoc decoder for
        -- the major types here to handle both the Major type 0 and 1.
        majorType01 =
            Bytes.unsignedInt8
                |> Bytes.andThen
                    (\a ->
                        if shiftRightBy 5 a == 0 then
                            unsigned a

                        else if shiftRightBy 5 a == 1 then
                            Bytes.map (\x -> negate x - 1) (unsigned (and a 31))

                        else
                            Bytes.fail
                    )
    in
    majorType01 |> Decoder


float : Decoder Float
float =
    let
        value a =
            if a == 25 then
                float16

            else if a == 26 then
                Bytes.float32 BE

            else if a == 27 then
                Bytes.float64 BE

            else
                Bytes.fail
    in
    majorType 7
        |> Bytes.andThen value
        |> Decoder


string : Decoder String
string =
    majorType 3
        |> Bytes.andThen unsigned
        |> Bytes.andThen Bytes.string
        |> Decoder


bytes : Decoder Bytes
bytes =
    majorType 2
        |> Bytes.andThen unsigned
        |> Bytes.andThen Bytes.bytes
        |> Decoder



{-------------------------------------------------------------------------------
                              Data-Structures
-------------------------------------------------------------------------------}


list : Decoder a -> Decoder (List a)
list (Decoder elem) =
    let
        step ( n, es ) =
            if n <= 0 then
                es |> List.reverse |> Bytes.Done |> Bytes.succeed

            else
                elem |> Bytes.map (\e -> Bytes.Loop ( n - 1, e :: es ))
    in
    majorType 4
        |> Bytes.andThen (\n -> Bytes.loop ( n, [] ) step)
        |> Decoder


dict : Decoder comparable -> Decoder a -> Decoder (Dict comparable a)
dict (Decoder key) (Decoder value) =
    let
        step ( n, es ) =
            if n <= 0 then
                es |> List.reverse |> Dict.fromList |> Bytes.Done |> Bytes.succeed

            else
                Bytes.map2 Tuple.pair key value
                    |> Bytes.map (\e -> Bytes.Loop ( n - 1, e :: es ))
    in
    majorType 5
        |> Bytes.andThen (\n -> Bytes.loop ( n, [] ) step)
        |> Decoder



{-------------------------------------------------------------------------------
                                 Internal
-------------------------------------------------------------------------------}


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
    Bytes.unsignedInt8
        |> Bytes.andThen
            (\a ->
                if shiftRightBy 5 a == k then
                    Bytes.succeed (and a 31)

                else
                    Bytes.fail
            )


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
