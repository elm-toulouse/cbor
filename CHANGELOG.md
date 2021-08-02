# Changelog 

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
