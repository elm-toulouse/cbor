module Cbor exposing (CborItem(..))

{-|

@docs CborItem

-}

import Bytes exposing (Bytes)
import Cbor.Tag exposing (Tag)


{-| A generic sum-type for representing any arbitrary CBOR item. See also
Cbor.Decode#any and Cbor.Encode#any
-}
type CborItem
    = CborInt Int
    | CborBytes Bytes
    | CborString String
    | CborList (List CborItem)
    | CborMap (List ( CborItem, CborItem ))
    | CborTag Tag
    | CborBool Bool
    | CborFloat Float
    | CborNull
    | CborUndefined
