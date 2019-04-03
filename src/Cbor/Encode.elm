module Cbor.Encode exposing
    ( Encoder, encode
    , bool, int
    )

{-| The Concise Binary Object Representation (CBOR) is a data format whose design
goals include the possibility of extremely small code size, fairly small message
size, and extensibility without the need for version negotiation. These design
goals make it different from earlier binary serializations such as ASN.1 and
MessagePack.


## Encoder

@docs Encoder, encode


## Primitives

@docs bool, int, float, string, bytes


## Data Structures

@docs list, dict, pair, maybe


## Mapping

@docs succeed, fail, andThen, map, map2, map3, map4, map5


## Tagging

@docs Tag, tag, tagged

-}

import Bitwise exposing (shiftRightBy)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as Bytes



{------------------------------------------------------------------------------
                                  Encoder
------------------------------------------------------------------------------}


type Encoder
    = Encoder Bytes.Encoder


encode : Encoder -> Bytes
encode (Encoder e) =
    Bytes.encode e



{-------------------------------------------------------------------------------
                                 Primitives
-------------------------------------------------------------------------------}


bool : Bool -> Encoder
bool n =
    Encoder <|
        case n of
            False ->
                Bytes.unsignedInt8 0xF4

            True ->
                Bytes.unsignedInt8 0xF5


int : Int -> Encoder
int n =
    Encoder <|
        if n < 0x18 then
            Bytes.unsignedInt8 n

        else if n < 0x0100 then
            Bytes.sequence
                [ Bytes.unsignedInt8 24
                , Bytes.unsignedInt8 n
                ]

        else if n < 0x00010000 then
            Bytes.sequence
                [ Bytes.unsignedInt8 25
                , Bytes.unsignedInt16 BE n
                ]

        else if n < 0x0000000100000000 then
            Bytes.sequence
                [ Bytes.unsignedInt8 26
                , Bytes.unsignedInt32 BE n
                ]

        else
            Bytes.sequence
                [ Bytes.unsignedInt8 27
                , Bytes.unsignedInt32 BE (n // 0x0000000100000000)
                , Bytes.unsignedInt32 BE (shiftRightBy 32 n)
                ]
