module Cbor.Encode exposing
    ( Encoder, encode, sequence, maybe, keyValue
    , bool, int, float, string, bytes, null, undefined
    , float16, float32, float64
    , list, length, associativeList, dict, size
    , Step, record, fields, field, optionalField, tuple, elems, elem
    , beginString, beginBytes, beginList, beginDict, break
    , tag, tagged
    , any, raw
    )

{-| The Concise Binary Object Representation (CBOR) is a data format whose design
goals include the possibility of extremely small code size, fairly small message
size, and extensibility without the need for version negotiation. These design
goals make it different from earlier binary serializations such as ASN.1 and
MessagePack.


## Encoder

@docs Encoder, encode, sequence, maybe, keyValue


## Primitives

@docs bool, int, float, string, bytes, null, undefined


## Fancier Primitives

@docs float16, float32, float64


## Simple Data-Structures

@docs list, length, associativeList, dict, size


## Records & Tuples

@docs Step, record, fields, field, optionalField, tuple, elems, elem


## Streaming

Four CBOR items (arrays, maps, byte strings, and text strings) can be encoded
with an indefinite length. This is useful if the encoding of the item needs to
begin before the number of items inside the array or map, or the total length
of the string, is known. (The application of this is often referred to as
"streaming" within a data item.)

> **NOTE**:
>
> Indefinite-length arrays and maps are dealt with differently than
> indefinite-length byte strings and text strings.

@docs beginString, beginBytes, beginList, beginDict, break


## Tagging

@docs tag, tagged


## Debugging

@docs any, raw

-}

import Bitwise exposing (and, or, shiftLeftBy, shiftRightBy)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D
import Bytes.Encode as E
import Bytes.Floating.Encode as E
import Cbor exposing (CborItem(..))
import Cbor.Tag exposing (Tag(..))
import Dict exposing (Dict)



{------------------------------------------------------------------------------
                                  Encoder
------------------------------------------------------------------------------}


{-| Describes how to encode a data structure or a type into binary CBOR
-}
type Encoder
    = Encoder E.Encoder


{-| Turn a CBOR `Encoder` into [`Bytes`](https://package.elm-lang.org/packages/elm/bytes/latest/Bytes#Bytes).
-}
encode : Encoder -> Bytes
encode (Encoder e) =
    E.encode e


{-| Combine a bunch of encoders sequentially.
-}
sequence : List Encoder -> Encoder
sequence =
    List.map (\(Encoder e) -> e) >> E.sequence >> Encoder


{-| Optionally encode a value. `Nothing` is encoded as [`null`](#null).
-}
maybe : (a -> Encoder) -> Maybe a -> Encoder
maybe encodeA m =
    case m of
        Nothing ->
            null

        Just a ->
            encodeA a


{-| Encode a key-value pair as a sequence of a key and a value. This is merely a
shorthand for a `sequence` on a 2-tuple.

    E.keyValue E.string E.int ( "a", 14 )
        == E.sequence
            [ E.string "a"
            , E.int 14
            ]

-}
keyValue : (a -> Encoder) -> (b -> Encoder) -> ( a, b ) -> Encoder
keyValue encodeA encodeB ( a, b ) =
    sequence
        [ encodeA a
        , encodeB b
        ]



{-------------------------------------------------------------------------------
                                 Primitives
-------------------------------------------------------------------------------}


{-| Encode booleans.

    E.bool False == Bytes<0xF4>

    E.bool True == Bytes<0xF5>

-}
bool : Bool -> Encoder
bool n =
    Encoder <|
        case n of
            False ->
                E.unsignedInt8 0xF4

            True ->
                E.unsignedInt8 0xF5


{-| Encode integers from `-9007199254740992` (`-2⁵³`) to `9007199254740991` (`2⁵³ - 1`).

    E.int 0 == Bytes<0x00>

    E.int 1337 == Bytes<0x19, 0x05, 0x39>

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

    E.float 0 == Bytes<0xFB, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>

    E.float -4.1 == Bytes<0xFB, 0xC0, 0x10, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66>

> **NOTE**:
>
> This is an alias for [`float64`](#float64).

-}
float : Float -> Encoder
float =
    float64


{-| Encode a [`String`](https://package.elm-lang.org/packages/elm/core/latest/String#String) of fixed size as a (definite) CBOR text string.

    E.string "" == Bytes<0x60>

    E.string "IETF" == Bytes <0x64, 0x49, 0x45, 0x54, 0x46>

    E.string "🌈" == Bytes<0x64, 0xF0, 0x9F, 0x8C, 0x88>

-}
string : String -> Encoder
string str =
    Encoder <|
        E.sequence
            [ unsigned 3 (E.getStringWidth str)
            , E.string str
            ]


{-| Encode raw [`Bytes`](https://package.elm-lang.org/packages/elm/bytes/latest/Bytes#Bytes) of fixed size as a (definite) CBOR byte string.

    E.bytes Bytes<> == Bytes<0x40>

    E.bytes Bytes<0x01, 0x02, 0x03, 0x04> = Bytes<0x44, 0x01, 0x02, 0x03, 0x04>

-}
bytes : Bytes -> Encoder
bytes bs =
    Encoder <|
        E.sequence
            [ unsigned 2 (Bytes.width bs)
            , E.bytes bs
            ]


{-| Create a CBOR `null` value. This can be decoded using [`Cbor.decode.maybe`](https://package.elm-lang.org/packages/elm-toulouse/cbor/latest/Cbor-Decode#maybe).

    E.null == Bytes<0xF6>

-}
null : Encoder
null =
    Encoder <| E.unsignedInt8 0xF6


{-| Create a CBOR `undefined` value. This can be decoded using [`Cbor.decode.maybe`](https://package.elm-lang.org/packages/elm-toulouse/cbor/latest/Cbor-Decode#maybe).

    E.undefined == Bytes<0xF7>

-}
undefined : Encoder
undefined =
    Encoder <| E.unsignedInt8 0xF7



{-------------------------------------------------------------------------------
                              Fancier Primitives
-------------------------------------------------------------------------------}


{-| Encode floating numbers with half-precision (16-bit).

    E.float16 0.0 == Bytes<0xF9, 0x00, 0x00>

    E.float16 -0.0 == Bytes<0xF9, 0x80, 0x00>

    E.float16 1.5 == Bytes<0xF9, 0x3E, 0x00>

-}
float16 : Float -> Encoder
float16 n =
    Encoder <|
        E.sequence
            [ majorType 7 25
            , E.float16 BE n
            ]


{-| Encode floating numbers with simple precision (32-bit).

    E.float32 0.0 == Bytes<0xFA, 0x00, 0x00, 0x00, 0x00>

    E.float32 3.4028234663852886e38 == Bytes<0xFA, 0x7F, 0x7F, 0xFF, 0xFF>

-}
float32 : Float -> Encoder
float32 n =
    Encoder <|
        E.sequence
            [ majorType 7 26
            , E.float32 BE n
            ]


{-| Encode floating numbers with double precision (64-bit).

    E.float64 0 == Bytes<0xFB, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>

    E.float64 -4.1 == Bytes<0xFB, 0xC0, 0x10, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66>

-}
float64 : Float -> Encoder
float64 n =
    Encoder <|
        E.sequence
            [ majorType 7 27
            , E.float64 BE n
            ]



{-------------------------------------------------------------------------------
                               Data-Structures
-------------------------------------------------------------------------------}


{-| Turn a `List` into a (definite) CBOR array

    E.list E.int [1,2,3] == Bytes<0x83, 0x01, 0x02, 0x03>

-}
list : (a -> Encoder) -> List a -> Encoder
list e xs =
    sequence <|
        Encoder (unsigned 4 (List.length xs))
            :: List.map e xs


{-| Encode a (definite) list length only. This may be useful to stream a
definite list or simply, to have even more fine-grained control over the
creation of a definite list.

    E.sequence
        [ E.length 2
        , E.int 1
        , E.int 2
        ]
        == E.list E.int [ 1, 2 ]

-}
length : Int -> Encoder
length =
    Encoder << unsigned 4


{-| Turn a `(key, value)` associative list into a (definite) CBOR map. Note
that, if keys are `comparable`, you should consider using a [`Dict`](https://package.elm-lang.org/packages/elm/core/latest/Dict#Dict) and
[`dict`](#dict) instead.
-}
associativeList : (k -> Encoder) -> (v -> Encoder) -> List ( k, v ) -> Encoder
associativeList k v xs =
    sequence <|
        Encoder (unsigned 5 (List.length xs))
            :: List.map (keyValue k v) xs


{-| Turn a [`Dict`](https://package.elm-lang.org/packages/elm/core/latest/Dict#Dict) into a (definite) CBOR map.

    E.dict E.string E.int (Dict.fromList [ ( "a", 1 ), ( "b", 2 ) ])
        == Bytes<0xA2, 0x61, 0x61, 0x01, 0x61, 0x62, 0x02>

-}
dict : (k -> Encoder) -> (v -> Encoder) -> Dict k v -> Encoder
dict k v =
    associativeList k v << Dict.toList


{-| Encode a (definite) dict size only. This may be useful to stream a
definite dict or simply, to have even more fine-grained control over the
creation of a definite dict.
-}
size : Int -> Encoder
size =
    Encoder << unsigned 5



{-------------------------------------------------------------------------------
                                Records / Tuples
-------------------------------------------------------------------------------}


{-| An intermediate (opaque) step in the encoding of a record or tuple. See
[`record`](#record) or [`tuple`](#tuple) for more detail.
-}
type Step k result
    = Step { steps : List Encoder, encodeKey : k -> Encoder, this : result }


{-| Encode a record as a (definite) CBOR map. Keys in the map can be arbitrary
CBOR but are expected to be homogeneous across the record.

    type alias Album =
        { artist : String
        , title : String
        , label : Maybe String
        }

    -- In this example, we use compact integer as keys.
    encodeAlbumCompact : Album -> E.Encoder
    encodeAlbumCompact =
        E.record E.int <|
            E.fields
                >> E.field 0 E.string .artist
                >> E.field 1 E.string .title
                >> E.optionalField 2 E.string .genre

    -- In this example, we use more verbose string keys.
    encodeAlbumVerbose : Album -> E.Encoder
    encodeAlbumVerbose =
        E.record E.string <|
            E.fields
                >> E.field "artist" E.string .artist
                >> E.field "title" E.string .title
                >> E.optionalField "label" E.string .genre

-}
record : (k -> Encoder) -> (Step k record -> Step k record) -> record -> Encoder
record encodeKey step this =
    let
        (Step { steps }) =
            step <|
                Step { steps = [], encodeKey = encodeKey, this = this }
    in
    sequence (size (List.length steps // 2) :: List.reverse steps)


{-| A helper that makes writing record encoders nicer. It is equivalent to
`identity`, but let us align encoders to fight compulsory OCDs.
-}
fields : Step k record -> Step k record
fields =
    identity


{-| Encode a field of record and step through the encoding. See [`record`](#record)
for detail about usage.
-}
field : k -> (field -> Encoder) -> (record -> field) -> Step k record -> Step k record
field k encodeValue extract (Step { steps, encodeKey, this }) =
    Step
        { steps = encodeValue (extract this) :: encodeKey k :: steps
        , encodeKey = encodeKey
        , this = this
        }


{-| Encode an optional field of record and step through the encoding. See [`record`](#record)
for detail about usage.

> **NOTE**:
>
> When the value is `Nothing`, the field (and its key) is completely omitted
> from the final record.

-}
optionalField : k -> (field -> Encoder) -> (record -> Maybe field) -> Step k record -> Step k record
optionalField k encodeValue extract ((Step { steps, encodeKey, this }) as step) =
    case extract this of
        Nothing ->
            step

        Just a ->
            field k encodeValue (always a) step


{-| Encode a record / tuple as a (definite) CBOR array.

    type alias Track =
        { title : String
        , duration : Int
        }

    encodeTrack : Track -> E.Encoder
    encodeTrack =
        E.tuple <|
            E.elems
                >> E.elem E.string .title
                >> E.elem E.int .duration

-}
tuple : (Step Never tuple -> Step Never tuple) -> tuple -> Encoder
tuple step this =
    let
        (Step { steps }) =
            step <|
                Step { steps = [], encodeKey = never, this = this }
    in
    sequence (length (List.length steps) :: List.reverse steps)


{-| A helper that makes writing tuple encoders nicer. It is equivalent to
`identity`, but let us align encoders to fight compulsory OCDs.
-}
elems : Step Never tuple -> Step Never tuple
elems =
    identity


{-| Encode an elements of a tuple and step through the encoding. See [`tuple`](#tuple)
for detail about usage.
-}
elem : (elem -> Encoder) -> (tuple -> elem) -> Step Never tuple -> Step Never tuple
elem encodeElem extract (Step { steps, encodeKey, this }) =
    Step
        { steps = encodeElem (extract this) :: steps
        , encodeKey = encodeKey
        , this = this
        }



{-------------------------------------------------------------------------------
                                 Streaming
-------------------------------------------------------------------------------}


{-| Encode a [`Bytes`](https://package.elm-lang.org/packages/elm/bytes/latest/Bytes#Bytes)
of indefinite length in chunks. This indicates the beginning of multiple calls
to [`bytes`](#bytes), followed by a [`break`](#break) to signal the end of the
stream. For example:

    E.sequence
        [ E.beginBytes
        , E.bytes Bytes<0x01, 0x02>
        , E.bytes Bytes<0x03, 0x04>
        , E.break
        ]

-}
beginBytes : Encoder
beginBytes =
    Encoder <| majorType 2 tBEGIN


{-| Encode a [`String`](https://package.elm-lang.org/packages/elm/core/latest/String#String)
of indefinite length in chunks. This indicates the beginning of multiple calls
to [`string`](#string), followed by a [`break`](#break) to signal the end of the
stream. For example:

    E.sequence
        [ E.beginString
        , E.string "elm"
        , E.string "rocks"
        , E.string "!"
        , E.break
        ]

-}
beginString : Encoder
beginString =
    Encoder <| majorType 3 tBEGIN


{-| Encode a `List` of indefinite length. This indicates the beginning of
multiple calls for encoding elements, followed by a [`break`](#break) to signal
the end of the stream. For example:

    E.sequence
        [ E.beginList
        , E.string "elm"
        , E.string "rocks"
        , E.string "!"
        , E.break
        ]

-}
beginList : Encoder
beginList =
    Encoder <| majorType 4 tBEGIN


{-| Encode a [`Dict`](https://package.elm-lang.org/packages/elm/core/latest/Dict#Dict)
of indefinite length. This indicates the beginning of multiple calls for
encoding pairs of elements, followed by a [`break`](#break) to signal the end of
the stream. For example:

    E.sequence
        [ E.beginDict
        , E.keyValue E.int E.string ( 1, "elm" )
        , E.keyValue E.int E.string ( 2, "rocks" )
        , E.keyValue E.int E.string ( 3, "!" )
        , E.break
        ]

-}
beginDict : Encoder
beginDict =
    Encoder <| majorType 5 tBEGIN


{-| Encode termination of an indefinite structure. See
[`beginString`](#beginString), [`beginBytes`](#beginBytes),
[`beginList`](#beginList), [`beginDict`](#beginDict) for detail about usage.
-}
break : Encoder
break =
    Encoder <| E.unsignedInt8 tBREAK



{-------------------------------------------------------------------------------
                                  Debugging
-------------------------------------------------------------------------------}


{-| Encode any generic CBOR item. This is particularly useful when dealing with
heterogeneous data structures (e.g. tuples).

    E.list E.any [ CborInt 42, CborBool True, CborString "awesome!" ]

-}
any : CborItem -> Encoder
any item =
    case item of
        CborInt i ->
            int i

        CborBytes bs ->
            bytes bs

        CborString str ->
            string str

        CborList xs ->
            list any xs

        CborMap xs ->
            associativeList any any xs

        CborTag t ->
            tag t

        CborBool b ->
            bool b

        CborFloat f ->
            float f

        CborNull ->
            null

        CborUndefined ->
            undefined


{-| Unsafe encoder to inject any arbitrary bytes into the encoding sequence. **Do
not use** unless you know what you're doing, this may result in invalid CBOR
encoding!
-}
raw : Bytes -> Encoder
raw =
    E.bytes >> Encoder



{-------------------------------------------------------------------------------
                                    Tagging
-------------------------------------------------------------------------------}


{-| Encode a particular [`Tag`](https://package.elm-lang.org/packages/elm-toulouse/cbor/latest/Cbor-Tag#Tag) as a CBOR tag prefix.
-}
tag : Tag -> Encoder
tag t =
    Encoder <|
        case t of
            StandardDateTime ->
                unsigned 6 0

            EpochDateTime ->
                unsigned 6 1

            PositiveBigNum ->
                unsigned 6 2

            NegativeBigNum ->
                unsigned 6 3

            DecimalFraction ->
                unsigned 6 4

            BigFloat ->
                unsigned 6 5

            Base64UrlConversion ->
                unsigned 6 21

            Base64Conversion ->
                unsigned 6 22

            Base16Conversion ->
                unsigned 6 23

            Cbor ->
                unsigned 6 24

            Uri ->
                unsigned 6 32

            Base64Url ->
                unsigned 6 33

            Base64 ->
                unsigned 6 34

            Regex ->
                unsigned 6 35

            Mime ->
                unsigned 6 36

            IsCbor ->
                unsigned 6 55799

            Unknown i ->
                unsigned 6 i


{-| Helper to quickly a tagged value

    E.tagged t encodeA a == E.sequence [ E.tag t, encodeA a ]

-}
tagged : Tag -> (a -> Encoder) -> a -> Encoder
tagged t encodeA a =
    sequence [ tag t, encodeA a ]



{-------------------------------------------------------------------------------
                                   Internals
-------------------------------------------------------------------------------}


{-| Marks the beginning of an indefinite structure
-}
tBEGIN : Int
tBEGIN =
    31


{-| Marks the end of an indefinite structure
-}
tBREAK : Int
tBREAK =
    0xFF


{-| Encode a major type and its additional payload. Major types are encoded
using 3 bits in a single byte. The meaning given to the additional value depends
on the major type itself.

       Major type -----*                  *---------- 5-bit additional data
                       |                  |
                       |                  |
                /------------\ /----------------------\
                 2⁷ | 2⁶ | 2⁵ | 2⁴ | 2³ | 2² | 2¹ | 2⁰

-}
majorType : Int -> Int -> E.Encoder
majorType major payload =
    E.unsignedInt8 <| or payload (shiftLeftBy 5 major)


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
unsigned : Int -> Int -> E.Encoder
unsigned major n =
    if n < 24 then
        majorType major n

    else if n < 256 then
        E.sequence
            [ majorType major 24
            , E.unsignedInt8 n
            ]

    else if n < 65536 then
        E.sequence
            [ majorType major 25
            , E.unsignedInt16 BE n
            ]

    else if n < 4294967296 then
        E.sequence
            [ majorType major 26
            , E.unsignedInt32 BE n
            ]

    else
        E.sequence
            [ majorType major 27
            , E.unsignedInt32 BE (n // 4294967296)
            , E.unsignedInt32 BE n
            ]
