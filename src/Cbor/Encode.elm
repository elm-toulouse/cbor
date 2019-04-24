module Cbor.Encode exposing
    ( Encoder, encode
    , bool, int, float
    , float16, float32, float64
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


## Fancier Primitives

@docs float16, float32, float64


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


{-| Encode booleans
-}
bool : Bool -> Encoder
bool n =
    Encoder <|
        case n of
            False ->
                Bytes.unsignedInt8 0xF4

            True ->
                Bytes.unsignedInt8 0xF5


{-| Encode integers from `-9007199254740992` (`-2⁵³`) to `9007199254740991` (`2⁵³ - 1`)
-}
int : Int -> Encoder
int n =
    Encoder <|
        if n <= -9007199254740992 then
            unsigned 1 9007199254740991

        else if n < 0 then
            unsigned 1 (negate n - 1)

        else
            unsigned 0 n


{-| Encode floating numbers with maximum precision (64-bit).

> NOTE: This is an alias for 'float64'

-}
float : Float -> Encoder
float =
    float64



{-------------------------------------------------------------------------------
                              Fancier Primitives
-------------------------------------------------------------------------------}


{-| Encode floating numbers with half-precision (16-bit)
-}
float16 : Float -> Encoder
float16 _ =
    Debug.todo "float16"


{-| Encode floating numbers with simple precision (32-bit)
-}
float32 : Float -> Encoder
float32 n =
    Encoder <|
        Bytes.sequence
            [ majorType 7 26
            , Bytes.float32 BE n
            ]


{-| Encode floating numbers with double precision (64-bit)
-}
float64 : Float -> Encoder
float64 n =
    Encoder <|
        Bytes.sequence
            [ majorType 7 27
            , Bytes.float64 BE n
            ]



{-------------------------------------------------------------------------------
                                 Internals
-------------------------------------------------------------------------------}


{-| Encode a major type and its additional payload. Major types are encoded
using 3 bits in a single byte. The meaning given to the additional value depends
on the major type itself.

       Major type -----*                  *---------- 5-bit additional data
                       |                  |
                       |                  |
                /------------\ /----------------------\
                 2⁷ | 2⁶ | 2⁵ | 2⁴ | 2³ | 2² | 2¹ | 2⁰

-}
majorType : Int -> Int -> Bytes.Encoder
majorType major payload =
    Bytes.unsignedInt8 <| or payload (shiftLeftBy 5 major)


{-| Encode an unsigned int using the given major type and payload.

Small ints (< 24) are directly encoded as part of the major type payload, and do
no require extra bytes. Biggers values requires more bytes, the number of
extra bytes being specified by the payload.

    value                   | payload | description
    ---                     | ---     | ---
    0 <= n < 24             | n       | value encoded directly in the payload
    24 <= n < 256           | 24      | value encoded with 1 extra byte (int8)
    256 <= n < 65536        | 25      | value encoded with 2 extra bytes (int16)
    65536 <= n < 4294967296 | 26      | value encoded with 4 extra bytes (int32)
    n >= 4294967296         | 27      | value encoded with 8 extra bytes (int64)

Note that, we don't have any way to encode unsigned int64 in Elm, so we
artificially _emulate_ this by splitting the number in two, encoding both part
on 32-bit.

-}
unsigned : Int -> Int -> Bytes.Encoder
unsigned major n =
    if n < 24 then
        majorType major n

    else if n < 256 then
        Bytes.sequence
            [ majorType major 24
            , Bytes.unsignedInt8 n
            ]

    else if n < 65536 then
        Bytes.sequence
            [ majorType major 25
            , Bytes.unsignedInt16 BE n
            ]

    else if n < 4294967296 then
        Bytes.sequence
            [ majorType major 26
            , Bytes.unsignedInt32 BE n
            ]

    else
        Bytes.sequence
            [ majorType major 27
            , Bytes.unsignedInt32 BE (n // 4294967296)
            , Bytes.unsignedInt32 BE n
            ]
