module Cbor.Tag exposing (Tag(..))

{-| Optional semantic tags as specified in the RFC 7049. Tags can be used to
give an extra meaning to a generic piece of data that follows it. For instance,
one could encode a bignum as a raw byte string and add a corresponding tag 0x02
to indicates what meaning can be given to that bytestring.

@docs Tag

-}


{-| Known CBOR tag from the specification. For custom tags, use 'Unknown' as a
constructor
-}
type Tag
    = StandardDateTime
    | EpochDateTime
    | PositiveBigNum
    | NegativeBigNum
    | DecimalFraction
    | BigFloat
    | Base64UrlConversion
    | Base64Conversion
    | Base16Conversion
    | Cbor
    | Uri
    | Base64Url
    | Base64
    | Regex
    | Mime
    | IsCbor
    | Unknown Int
