module CBOR.Decode exposing
    ( Decoder(..), decodeBytes
    , int, bytes
    , list
    )

{-| The Concise Binary Object Representation (CBOR) is a data format whose design
goals include the possibility of extremely small code size, fairly small message
size, and extensibility without the need for version negotiation. These design
goals make it different from earlier binary serializations such as ASN.1 and
MessagePack.


## Decoder

@docs Decoder, decodeBytes


## Primitives

@docs int, bytes


## Data Structures

@docs list

-}

import Bitwise exposing (shiftRightBy)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Bytes
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
        majorType =
            Bytes.unsignedInt8
    in
    majorType
        |> Bytes.andThen
            (\a ->
                if shiftRightBy 5 a == 0 then
                    -- Major type 0: an unsigned integer
                    unsigned a

                else if shiftRightBy 5 a == 1 then
                    -- Major type 1: a negative integer
                    Bytes.map (\x -> negate x - 1) (unsigned (a - 2 ^ 5))

                else
                    Bytes.fail
            )
        |> Decoder


bytes : Decoder Bytes
bytes =
    let
        majorType =
            Bytes.unsignedInt8
                |> Bytes.andThen
                    (\a ->
                        -- Major type 2:  a byte string
                        if shiftRightBy 5 a == 2 then
                            Bytes.succeed (a - 2 ^ 6)

                        else
                            Bytes.fail
                    )
    in
    majorType
        |> Bytes.andThen unsigned
        |> Bytes.andThen Bytes.bytes
        |> Decoder



{-------------------------------------------------------------------------------
                              Data-Structures
-------------------------------------------------------------------------------}


list : Decoder a -> Decoder (List a)
list (Decoder decodeElem) =
    let
        majorType =
            Bytes.unsignedInt8
                |> Bytes.andThen
                    (\a ->
                        -- Major type 4: an array of data items
                        if shiftRightBy 5 a == 4 then
                            Bytes.succeed (a - 2 ^ 7)

                        else
                            Bytes.fail
                    )

        step ( n, es ) =
            if n <= 0 then
                es |> List.reverse |> Bytes.Done |> Bytes.succeed

            else
                decodeElem |> Bytes.map (\e -> Bytes.Loop ( n - 1, e :: es ))
    in
    majorType
        |> Bytes.andThen (\n -> Bytes.loop ( n, [] ) step)
        |> Decoder



{-------------------------------------------------------------------------------
                                 Internal
-------------------------------------------------------------------------------}


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
