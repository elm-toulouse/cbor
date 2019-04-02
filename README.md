# 🤖 elm-cbor(g) [![](https://img.shields.io/elm-package/v/elm-toulouse/elm-cbor.svg?style=for-the-badge)](https://package.elm-lang.org/packages/elm-toulouse/elm-cbor/latest/) ![](https://img.shields.io/coveralls/github/elm-toulouse/elm-cbor.svg?style=for-the-badge)


[**RFC 7049**](https://tools.ietf.org/html/rfc7049)

>   The Concise Binary Object Representation (CBOR) is a data format
>   whose design goals include the possibility of extremely small code
>   size, fairly small message size, and extensibility without the need
>   for version negotiation.  These design goals make it different from
>   earlier binary serializations such as ASN.1 and MessagePack.

## Getting Started

### Installation

```
elm install elm-toulouse/elm-cbor
```

### Usage

```elm
import Url exposing (Url)
import Cbor.Decode as D


type alias Album =
    { artist : String
    , title : String
    , year : Int
    , tracks : List ( String, Duration )
    , links : List Url
    }

type Duration
    = Duration Int

-- DECODER

decodeAlbum : D.Decoder Album
decodeAlbum =
    let
        url =
            D.string
                |> D.map Url.fromString
                |> D.andThen (Maybe.map D.succeed >> Maybe.withDefault D.fail)

        track =
            D.pair D.string (D.map Duration D.int)
    in
    D.map5 Album
        D.string
        D.string
        D.int
        (D.list track)
        (D.list url)
```

## Changelog

[CHANGELOG.md](CHANGELOG.md)

---

<p align="center"><img src="https://img.shields.io/github/license/elm-toulouse/elm-cbor.svg?style=for-the-badge" /> </p>
