module CBOR.Decode exposing (Decoder, decode, decodeBytes, decodeInt, decodeList)

import Bitwise exposing (shiftRightBy)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Bytes
import Tuple exposing (first)


type Decoder a
    = Decoder (Bytes.Decoder a)


decode : Decoder a -> Bytes -> Maybe a
decode (Decoder decoder) =
    Bytes.decode decoder


{-| Major type 0: an unsigned integer & Major type 1: a negative integer
-}
decodeInt : Decoder Int
decodeInt =
    let
        majorType =
            Bytes.unsignedInt8
    in
    majorType
        |> Bytes.andThen
            (\a ->
                if shiftRightBy 5 a == 0 then
                    decodeUnsigned a

                else if shiftRightBy 5 a == 1 then
                    Bytes.map (\x -> negate x - 1) (decodeUnsigned (a - 2 ^ 5))

                else
                    Bytes.fail
            )
        |> Decoder


{-| Major type 2: a byte string
-}
decodeBytes : Decoder Bytes
decodeBytes =
    let
        majorType =
            Bytes.unsignedInt8
                |> Bytes.andThen
                    (\a ->
                        if shiftRightBy 5 a == 2 then
                            Bytes.succeed (a - 2 ^ 6)

                        else
                            Bytes.fail
                    )
    in
    majorType
        |> Bytes.andThen decodeUnsigned
        |> Bytes.andThen Bytes.bytes
        |> Decoder


{-| Major type 4: an array of data items.
-}
decodeList : Decoder a -> Decoder (List a)
decodeList (Decoder decodeElem) =
    let
        majorType =
            Bytes.unsignedInt8
                |> Bytes.andThen
                    (\a ->
                        if shiftRightBy 5 a == 4 then
                            -- NOTE list length encoded on 5 last bits
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


decodeUnsigned : Int -> Bytes.Decoder Int
decodeUnsigned a =
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
