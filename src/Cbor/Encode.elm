module Cbor.Encode exposing
    ( Encoder, encode, sequence
    , bool, int, float, string, bytes, null
    , float16, float32, float64
    , list, dict, pair
    , beginStrings, beginBytes, beginList, beginDict, break
    , tag, tagged
    )

{-| The Concise Binary Object Representation (CBOR) is a data format whose design
goals include the possibility of extremely small code size, fairly small message
size, and extensibility without the need for version negotiation. These design
goals make it different from earlier binary serializations such as ASN.1 and
MessagePack.


## Encoder

@docs Encoder, encode, sequence


## Primitives

@docs bool, int, float, string, bytes, null


## Fancier Primitives

@docs float16, float32, float64


## Data Structures

@docs list, dict, pair


## Streaming

Four CBOR items (arrays, maps, byte strings, and text strings) can be encoded
with an indefinite length. This is useful if the encoding of the item needs to
begin before the number of items inside the array or map, or the total length
of the string, is known. (The application of this is often referred to as
"streaming" within a data item.)

> NOTE:
>
> Indefinite-length arrays and maps are dealt with differently than
> indefinite-length byte strings and text strings.

@docs beginStrings, beginBytes, beginList, beginDict, break


## Tagging

@docs tag, tagged

-}

import Bitwise exposing (and, or, shiftLeftBy, shiftRightBy)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as D
import Bytes.Encode as E
import Bytes.Floating.Encode as E
import Cbor.Tag exposing (Tag(..))
import Dict exposing (Dict)



{------------------------------------------------------------------------------
                                  Encoder
------------------------------------------------------------------------------}


{-| Describes how to encode a data structure or a type into binary CBOR
-}
type Encoder
    = Encoder E.Encoder


{-| Turn a CBOR 'Encoder' into 'Bytes'.

    import Cbor.Encode as E
    import Url exposing (Url)

    type alias Album =
        { artist : String
        , title : String
        , year : Int
        , tracks : List ( String, Duration )
        , links : List Url
        }

    type Duration
        = Duration Int

    encodeAlbum : Album -> E.Encoder
    encodeAlbum { artist, title, year, tracks, links } =
        let
            link =
                Url.toString >> E.string

            track =
                E.pair E.string (\(Duration d) -> E.int d)
        in
        E.sequence
            [ E.string artist
            , E.string title
            , E.int year
            , E.list track tracks
            , E.list link links
            ]

-}
encode : Encoder -> Bytes
encode (Encoder e) =
    E.encode e


{-| Combine a bunch of encoders
-}
sequence : List Encoder -> Encoder
sequence =
    List.map (\(Encoder e) -> e) >> E.sequence >> Encoder



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
                E.unsignedInt8 0xF4

            True ->
                E.unsignedInt8 0xF5


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


{-| Encode a 'String' of fixed size
-}
string : String -> Encoder
string str =
    Encoder <|
        E.sequence
            [ unsigned 3 (E.getStringWidth str)
            , E.string str
            ]


{-| Encode raw 'Bytes' of fixed size
-}
bytes : Bytes -> Encoder
bytes bs =
    Encoder <|
        E.sequence
            [ unsigned 2 (Bytes.width bs)
            , E.bytes bs
            ]


{-| Create a CBOR `null` value. This can be decoded using `maybe` from the
Cbor.Decode package
-}
null : Encoder
null =
    Encoder <| E.unsignedInt8 0xF6



{-------------------------------------------------------------------------------
                              Fancier Primitives
-------------------------------------------------------------------------------}


{-| Encode floating numbers with half-precision (16-bit)
-}
float16 : Float -> Encoder
float16 n =
    Encoder <|
        E.sequence
            [ majorType 7 25
            , E.float16 BE n
            ]


{-| Encode floating numbers with simple precision (32-bit)
-}
float32 : Float -> Encoder
float32 n =
    Encoder <|
        E.sequence
            [ majorType 7 26
            , E.float32 BE n
            ]


{-| Encode floating numbers with double precision (64-bit)
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


{-| Turn a 'List' into a CBOR array

    E.list E.int [1,2,3] == Bytes <0xTODO>

-}
list : (a -> Encoder) -> List a -> Encoder
list e xs =
    sequence <|
        Encoder (unsigned 4 (List.length xs))
            :: List.map e xs


{-| Turn a 2-'Tuple' into a CBOR array

    E.pair E.string E.int ("cbor", 14) == Bytes <0xTODO>

-}
pair : (a -> Encoder) -> (b -> Encoder) -> ( a, b ) -> Encoder
pair encodeA encodeB ( a, b ) =
    sequence
        [ encodeA a
        , encodeB b
        ]


{-| Turn a 'Dict' into a CBOR array

    E.dict E.string E.int (Dict.fromList [("a", 1), ("b", 2)])
        ==
    Bytes <0xA2, 0x61, 0x61, 0x01, 0x61, 0x62, 0x02>

Note that in CBOR, every data-structure are mostly arrays of items. As a
consequence, dictionnaries are encoded as a list of pairs (key, value).

-}
dict : (k -> Encoder) -> (v -> Encoder) -> Dict k v -> Encoder
dict k v d =
    sequence <|
        Encoder (unsigned 5 (Dict.size d))
            :: List.map (pair k v) (Dict.toList d)



{-------------------------------------------------------------------------------
                                 Streaming
-------------------------------------------------------------------------------}


{-| Encode a 'Bytes' of indefinite length. This indicates the beginning of
multiple calls to 'bytes', followed by a 'break' to signal the end of the
stream. For example:

    E.sequence
        [ E.beginBytes
        , E.bytes <0x01, 0x02>
        , E.bytes <0x03, 0x04>
        , E.break
        ]

-}
beginBytes : Encoder
beginBytes =
    Encoder <| majorType 2 tBEGIN


{-| Encode a 'String' of indefinite length. This indicates the beginning of
multiple calls to 'string', followed by a 'break' to signal the end of the
stream. For example:

    E.sequence
        [ E.beginStrings
        , E.string "elm"
        , E.string "rocks"
        , E.string "!"
        , E.break
        ]

-}
beginStrings : Encoder
beginStrings =
    Encoder <| majorType 3 tBEGIN


{-| Encode a 'List' of indefinite length. This indicates the beginning of
multiple calls for encoding elements, followed by a 'break' to signal the end of the
stream. For example:

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


{-| Encode a 'Dict' of indefinite length. This indicates the beginning of
multiple calls for encoding pairs of elements, followed by a 'break' to signal
the end of the stream. For example:

    E.sequence
        [ E.beginDict
        , E.pair E.int E.string ( 1, "elm" )
        , E.pair E.int E.string ( 2, "rocks" )
        , E.pair E.int E.string ( 3, "!" )
        , E.break
        ]

-}
beginDict : Encoder
beginDict =
    Encoder <| majorType 5 tBEGIN


{-| Encode termination of an indefinite structure.
-}
break : Encoder
break =
    Encoder <| E.unsignedInt8 tBREAK



{-------------------------------------------------------------------------------
                                    Tagging
-------------------------------------------------------------------------------}


{-| Encode a particular 'Tag' to binary CBOR
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


{-| Helper to quickly encode a tagged value

    tagged t encodeA a == sequence [ tag t, encodeA a ]

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
