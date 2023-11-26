module Bytes.Decode.Floating exposing (float16)

{-| Extra Floating-point binary representation for Elm

@docs float16

-}

import Bitwise exposing (and, or, shiftLeftBy, shiftRightBy)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode.Branchable as D
import Bytes.Encode as E


{-| Decode 2 bytes into a floating point number.
-}
float16 : Endianness -> D.Decoder Float
float16 endian =
    D.unsignedInt16 endian |> D.map (halfToFloat >> fromUnsignedInt32)



{-------------------------------------------------------------------------------
                                   Internals
-------------------------------------------------------------------------------}


{-| Convert a float16 representation (as an uint16) to a float32 representation

       exponent
              |        mantissa
    sign      |               |
       |      |               |
       |      |               |
       | /---------\/-------------------\
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
halfToFloat : Int -> Int
halfToFloat x =
    let
        s =
            x |> shiftRightBy 15 |> and 1

        e =
            x |> shiftRightBy 10 |> and 0x1F

        m =
            x |> and 0x03FF
    in
    if e == 0 then
        if m == 0 then
            s |> shiftLeftBy 31

        else
            iEEE754 <| renormalize { s = s, e = e, m = m }

    else if e == 31 then
        iEEE754 { s = s, e = 255, m = m |> shiftLeftBy 13 }

    else
        iEEE754 { s = s, e = e + 112, m = m |> shiftLeftBy 13 }


{-| De-normalize mantissa and then, renormalize it
-}
renormalize : { s : Int, e : Int, m : Int } -> { s : Int, e : Int, m : Int }
renormalize { s, e, m } =
    case m |> and 0x0400 of
        0 ->
            renormalize { s = s, e = e - 1, m = m |> shiftLeftBy 1 }

        _ ->
            { s = s, e = e + 113, m = m |> and -1025 }


{-| Leverage existing float32 decoder to _cast_ a unsigned int32 into a 'Float'
-}
fromUnsignedInt32 : Int -> Float
fromUnsignedInt32 =
    E.unsignedInt32 BE
        >> E.encode
        >> D.decode (D.float32 BE)
        >> Maybe.withDefault (0 / 0)


{-| Recompose a IEEE754 encoding with sign, exponent and mantissa into
a single number representing a floating number on 32 bytes.
-}
iEEE754 : { s : Int, e : Int, m : Int } -> Int
iEEE754 { s, e, m } =
    (s |> shiftLeftBy 31) |> or (e |> shiftLeftBy 23) |> or m
