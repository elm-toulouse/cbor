![](https://raw.githubusercontent.com/elm-toulouse/cbor/master/.github/logo.png)

---

[![](https://img.shields.io/elm-package/v/elm-toulouse/cbor.svg?style=for-the-badge)](https://package.elm-lang.org/packages/elm-toulouse/cbor/latest/) 
[![](https://img.shields.io/travis/elm-toulouse/cbor.svg?style=for-the-badge)](https://travis-ci.org/elm-toulouse/cbor/builds)
[![](https://img.shields.io/github/license/elm-toulouse/cbor.svg?style=for-the-badge)](https://github.com/elm-toulouse/cbor/blob/master/LICENSE)


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
import Url exposing (Url)
import Cbor.Decode as D
import Cbor.Encode as E


type alias Album =
    { artist : String
    , title : String
    , year : Int
    , tracks : List ( String, Duration )
    , links : List Url
    }

type Duration
    = Duration Int


-- ENCODER

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


-- DECODER

decodeAlbum : D.Decoder Album
decodeAlbum =
    let
        link =
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
        (D.list link)

```

## Testing

Code coverage available [here](https://elm-toulouse.github.io/cbor)

## Changelog

[CHANGELOG.md](https://github.com/elm-toulouse/cbor/blob/master/CHANGELOG.md)
