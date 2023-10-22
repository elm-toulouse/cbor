![](https://raw.githubusercontent.com/elm-toulouse/cbor/master/.github/logo.png)

---

[![](https://img.shields.io/elm-package/v/elm-toulouse/cbor.svg?style=for-the-badge)](https://package.elm-lang.org/packages/elm-toulouse/cbor/latest/)
[![](https://img.shields.io/travis/elm-toulouse/cbor.svg?style=for-the-badge&label=%F0%9F%94%A8%20Build)](https://travis-ci.org/elm-toulouse/cbor/builds)
[![](https://img.shields.io/codecov/c/gh/elm-toulouse/cbor.svg?color=e84393&label=%E2%98%82%EF%B8%8F%20Coverage&style=for-the-badge)](https://codecov.io/gh/elm-toulouse/cbor)
[![](https://img.shields.io/github/license/elm-toulouse/cbor.svg?style=for-the-badge&label=%20%F0%9F%93%84%20License)](https://github.com/elm-toulouse/cbor/blob/master/LICENSE)

[**RFC 7049**](https://tools.ietf.org/html/rfc7049)

> The Concise Binary Object Representation (CBOR) is a data format
> whose design goals include the possibility of extremely small code
> size, fairly small message size, and extensibility without the need
> for version negotiation.  These design goals make it different from
> earlier binary serializations such as ASN.1 and MessagePack.

## Getting Started

### Installation

```
elm install elm-toulouse/cbor
```

### Usage

```elm
import Cbor.Decode as D
import Cbor.Encode as E

type alias Album =
    { artist : String
    , title : String
    , genre : Maybe Genre
    , tracks : List Track
    , label : Maybe String
    }


type alias Track =
    { title : String
    , duration : Int
    }


type Genre
    = Electronic
    | Pop
    | Jazz
    | Metal


encodeAlbum : Album -> E.Encoder
encodeAlbum =
    E.record E.int <|
        E.fields
            >> E.field 0 E.string .artist
            >> E.field 1 E.string .title
            >> E.optionalField 2 encodeGenre .genre
            >> E.field 3 (E.list encodeTrack) .tracks
            >> E.optionalField 4 E.string .label


decodeAlbum : D.Decoder Album
decodeAlbum =
    D.record D.int Album <|
        D.fields
            >> D.field 0 D.string
            >> D.field 1 D.string
            >> D.optionalField 2 decodeGenre
            >> D.field 3 (D.list decodeTrack)
            >> D.optionalField 4 D.string


encodeTrack : Track -> E.Encoder
encodeTrack =
    E.tuple <|
        E.elems
            >> E.elem E.string .title
            >> E.elem E.int .duration


decodeTrack : D.Decoder Track
decodeTrack =
    D.tuple Track <|
        D.elems
            >> D.elem D.string
            >> D.elem D.int


encodeGenre : Genre -> E.Encoder
encodeGenre genre =
    case genre of
        Electronic ->
            E.int 0

        Pop ->
            E.int 1

        Jazz ->
            E.int 2

        Metal ->
            E.int 3


decodeGenre : D.Decoder Genre
decodeGenre =
    D.int
        |> D.andThen
            (\s ->
                case s of
                    0 ->
                        D.succeed Electronic

                    1 ->
                        D.succeed Pop

                    2 ->
                        D.succeed Jazz

                    3 ->
                        D.succeed Metal

                    _ ->
                        D.fail
            )
```

## Changelog

[CHANGELOG.md](https://github.com/elm-toulouse/cbor/blob/master/CHANGELOG.md)
