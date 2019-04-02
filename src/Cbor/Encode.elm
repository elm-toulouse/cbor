module Cbor.Encode exposing (encodeInt)

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as Encode exposing (Encoder)


encodeInt : Int -> Encoder
encodeInt n =
    if n < 0x18 then
        Encode.unsignedInt8 n

    else if n < 0x0100 then
        Encode.sequence
            [ Encode.unsignedInt8 24
            , Encode.unsignedInt8 n
            ]

    else if n < 0x00010000 then
        Encode.sequence
            [ Encode.unsignedInt8 25
            , Encode.unsignedInt16 BE n
            ]

    else if n < 0x0000000100000000 then
        Encode.sequence
            [ Encode.unsignedInt8 26
            , Encode.unsignedInt32 BE n
            ]

    else
        Encode.sequence
            [ Encode.unsignedInt8 27
            , Encode.unsignedInt32 BE (n // 0x0000000100000000)
            , Encode.unsignedInt32 BE (Bitwise.shiftRightBy 32 n)
            ]
