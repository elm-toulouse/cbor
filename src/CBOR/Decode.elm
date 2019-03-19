module CBOR.Decode exposing (Decoder, decode, decodeInt)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Bytes
import Tuple exposing (first)


type Decoder a
    = Decoder (Bytes.Decoder a)


decode : Decoder a -> Bytes -> Maybe a
decode (Decoder decoder) =
    Bytes.decode decoder


decodeInt : Decoder Int
decodeInt =
    let
        majorType =
            Bytes.unsignedInt8

        decodeValue a =
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
    in
    Decoder <| Bytes.andThen decodeValue majorType



{--------------------------------------------
                  Internal
--------------------------------------------}


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
