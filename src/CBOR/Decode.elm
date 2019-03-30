module CBOR.Decode exposing
    ( Decoder(..), decodeBytes
    , int, bytes, string
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

@docs int, bytes, string


## Data Structures

@docs list, dict

-}

import Bitwise exposing (and, shiftRightBy)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Bytes
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


int : Decoder Int
int =
    let
        -- NOTE Unfortunately, we don't have any 'Alternative'-ish instance on
        -- @Byte.Decoder@, or something if 'oneOf' to try several decoders in
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
                            Bytes.map (\x -> negate x - 1) (unsigned (and a majorTypeMask))

                        else
                            Bytes.fail
                    )
    in
    majorType01 |> Decoder


bytes : Decoder Bytes
bytes =
    majorType 2
        |> Bytes.andThen unsigned
        |> Bytes.andThen Bytes.bytes
        |> Decoder


string : Decoder String
string =
    majorType 3
        |> Bytes.andThen unsigned
        |> Bytes.andThen Bytes.string
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
                    <------------> <---------------------->
                     2⁷ | 2⁶ | 2⁵ | 2⁴ | 2³ | 2² | 2¹ | 2⁰

-}
majorType : Int -> Bytes.Decoder Int
majorType k =
    Bytes.unsignedInt8
        |> Bytes.andThen
            (\a ->
                if shiftRightBy 5 a == k then
                    Bytes.succeed (and a majorTypeMask)

                else
                    Bytes.fail
            )


{-| First three bits at 0, rest at 1
-}
majorTypeMask : Int
majorTypeMask =
    2 ^ 5 - 1


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
