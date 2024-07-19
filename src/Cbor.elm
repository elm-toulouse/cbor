module Cbor exposing (CborItem(..), Sign(..))

{-|

@docs CborItem, Sign

-}

import Bytes exposing (Bytes)
import Cbor.Tag exposing (Tag)


{-| A generic sum-type for representing any arbitrary CBOR item. See also
[`Cbor.Decode.any`](https://package.elm-lang.org/packages/elm-toulouse/cbor/latest/Cbor-Decode#any) and [`Cbor.Encode.any`](https://package.elm-lang.org/packages/elm-toulouse/cbor/latest/Cbor-Encode#any)
-}
type CborItem
    = CborInt32 Int
      -- CborInt64 (msb, lsb) -- msb gives the sign, lsb must be >= 0
    | CborInt64 ( Int, Int )
    | CborBytes Bytes
    | CborString String
    | CborList (List CborItem)
    | CborMap (List ( CborItem, CborItem ))
    | CborTag Tag CborItem
    | CborBool Bool
    | CborFloat Float
    | CborNull
    | CborUndefined


{-| A sum-type for representing signs of integer numbers. By convention, 0 is
always `Positive`.
-}
type Sign
    = Positive
    | Negative
