# Changelog

## v3.1.0 (2023-12-03)

### Added

#### Cbor.Decode

- New primitive `optionalElem` for decoding tuples of variable length.

  ```elm
  optionalElem :
    Decoder field
    -> Decoder (Step Never (Maybe field -> steps))
    -> Decoder (Step Never steps)
  ```

### Changed

#### Cbor.Decode

- `fail` no longer fails eagerly, but at a later stage when needed. This fixes the use of partial decoder in conjunction with `maybe`.

### Removed

N/A

## v3.0.0 (2023-10-27)

### Changed

#### Cbor

- `CborItem.CborTag` now necessarily carries another tagged `CborItem`.

## v2.1.0 (2023-10-27)

### Added

#### Cbor.Decode

- New primitive `traverse`:

  ```elm
  traverse :
      (a -> Decoder b)
      -> List a
      -> Decoder (List b)
  ```

#### Cbor.Encode

- New primitive `indefiniteList`:

  ```elm
  indefiniteList :
      (a -> Encoder) -> List a -> Encoder
  ```

- New primitive `optionalElem`:

  ```elm
  optionalElem :
      (elem -> Encoder)
      -> (tuple -> Maybe elem)
      -> Step Never tuple
      -> Step Never tuple
    ```

### Fixed

- Reviewed internal implementation of Cbor.Decode to, in particular, fix an
  issue with `Cbor.Decode.andThen` sometimes decoding more bytes than expected
  and causing decoder to fail prematurely.

## v2.0.0 (2023-10-22)

### Added

#### Cbor.Decode

- New primitives for decoding tuples:

  ```elm
  tuple :
      steps
      -> (Step Never steps -> Decoder (Step Never tuple))
      -> Decoder tuple

  elems :
      Step Never steps
      -> Decoder (Step Never steps)

  elem :
      Decoder field
      -> Decoder (Step Never (field -> steps))
      -> Decoder (Step Never steps)
  ```

- New primitives for decoding records:

  ```elm
  record :
      Decoder k
      -> steps
      -> (Step k steps -> Decoder (Step k record))
      -> Decoder record

  fields :
      Step k steps
      -> Decoder (Step k steps)

  field :
      k
      -> Decoder field
      -> Decoder (Step k (field -> steps))
      -> Decoder (Step k steps)

  optionalField :
      k
      -> Decoder field
      -> Decoder (Step k (Maybe field -> steps))
      -> Decoder (Step k steps)
  ```

- New primitives for manually decoding definite & indefinite structures:

  ```elm
  length : Decoder Int
  size : Decoder Int

  beginBytes : Decoder ()
  beginDict : Decoder ()
  beginList : Decoder ()
  beginString : Decoder ()
  break : Decoder ()
  ```

- New helpers for combining decoders:

  ```elm
  ignoreThen : Decoder a -> Decoder ignored -> Decoder a
  thenIgnore : Decoder ignored -> Decoder a -> Decoder a
  ```

#### Cbor.Encode

- New primitives for encoding tuples:

  ```elm
  tuple :
      (Step Never tuple
      -> Step Never tuple)
      -> tuple
      -> Encoder

  elems :
      Step Never tuple
      -> Step Never tuple
  elem :
      (elem -> Encoder)
      -> (tuple -> elem)
      -> Step Never tuple
      -> Step Never tuple
  ```

- New primitives for encoding records:

  ```elm
  record :
      (k -> Encoder)
      -> (Step k record -> Step k record)
      -> record
      -> Encoder


  fields :
      Step k record
      -> Step k record

  field :
      k
      -> (field -> Encoder)
      -> (record -> field)
      -> Step k record
      -> Step k record

  optionalField :
      k
      -> (field -> Encoder)
      -> (record -> Maybe field)
      -> Step k record
      -> Step k record
  ```

- New primitives for manually encoding definite structures:

  ```elm
  size : Int -> Encoder
  length : Int -> Encoder
  ```

- New helper for optionally encoding values:

  ```elm
  maybe : (a -> Encoder) -> Maybe a -> Encoder
  ```

### Changed

#### Cbor.Decode

- Changed `record` as described in the previous section.

#### Cbor.Encode

- Renamed `beginStrings` into `beginString`.
- Renamed `pair` into `keyValue`.
- Renamed `keyValueMap` into `associativeList`.

### Removed

#### Cbor.Decode

- Removed `array`, made obsolete by the new (more flexible) additions.

## v1.1.0 (2021-08-03)

### Added

#### Cbor

- New module `Cbor` exposing a generic `CborItem` sum-type.

#### Cbor.Decode

- New primitives added (see #1):

  ```elm
  keyValueMap : Decoder k -> Decoder v -> Decoder (List ( k, v ))

  array : Decoder a -> Decoder a
  record : Decoder a -> Decoder a

  any : Decoder CborItem
  raw : Decoder Bytes
  ```

#### Cbor.Encode

- New primitives added:

  ```elm
  keyValueMap : (k -> Encoder) -> (v -> Encoder) -> List ( k, v ) -> Encoder

  undefined : Encoder

  any : CborItem -> Encoder
  raw : Bytes -> Encoder
  ```

### Changed

N/A

### Removed

N/A

## v1.0.0 (2019-05-07)

### Added

- Initial RFC-7049 implementation
- Provide unit & fuzz tests
- Initial README with example usage

### Changed

N/A

### Removed

N/A
