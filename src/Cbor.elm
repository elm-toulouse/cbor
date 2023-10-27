module Cbor exposing (CborItem(..))

{-|

@docs CborItem

-}

import Bytes exposing (Bytes)
import Cbor.Tag exposing (Tag)


{-| A generic sum-type for representing any arbitrary CBOR item. See also
[`Cbor.Decode.any`](https://package.elm-lang.org/packages/elm-toulouse/cbor/latest/Cbor-Decode#any) and [`Cbor.Encode.any`](https://package.elm-lang.org/packages/elm-toulouse/cbor/latest/Cbor-Encode#any)
-}
type CborItem
    = CborInt Int
    | CborBytes Bytes
    | CborString String
    | CborList (List CborItem)
    | CborMap (List ( CborItem, CborItem ))
    | CborTag Tag CborItem
    | CborBool Bool
    | CborFloat Float
    | CborNull
    | CborUndefined
