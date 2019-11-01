# Blob

<img src="./assets/icon.png" alt="Blob icon" align=right width=128px />

[![license](https://img.shields.io/cran/l/blob?style=for-the-badge)](./COPYING.md)    [![license](https://img.shields.io/github/license/garameki/BSD3?style=for-the-badge)](./COPYING.md)

[![stars](https://img.shields.io/github/stars/mesabloo/blob?color=%23fdaa33&style=for-the-badge)](https://github.com/mesabloo/blob/stargazers)    [![forks](https://img.shields.io/github/forks/mesabloo/blob?color=%23654321&label=Forks&style=for-the-badge)](https://github.com/mesabloo/blob/network/members)

## <a name='Tableofcontents'></a>Table of contents

<!-- Automatically generated table of contents -->
*  [Table of contents](#Tableofcontents)
*  [About blob](#Aboutblob)
*  [Why blob?](#Whyblob)
*  [Roadmap](#Roadmap)
*  [Unicode](#Unicode)
*  [Examples](#Examples)
*  [Contributors](#Contributors)
*  [References, inspirations](#Referencesinspirations)

## <a name='Aboutblob'></a>About blob

[![programming language](https://img.shields.io/github/languages/top/mesabloo/blob?color=%20%235e5086&style=for-the-badge)](https://github.com/Mesabloo/blob/search?l=haskell)    [![code size](https://img.shields.io/github/languages/code-size/mesabloo/blob?color=%23123456&style=for-the-badge)]()

Blob is a small functional programming language entirely written using Haskell.

It is not aimed at being a used programming language, but more like a proof of concept, and to prove to myself I can achieve some great things.

## <a name='Whyblob'></a>Why blob?

Blob came to life after I had an idea: *“what if we could control how much resources are used?”*

In the beginning, I was willing to make an esoteric language, poorly designed, but functional.

Beginning with some C++, I built my own parser combinators library, an old crappy one, which in the end wasn't working that well, because of some strange things I am still unable to really understand.

[@felko](https://github.com/felko) introduced me to Haskell, and I wanted to give it a try for many months. That was the perfect time to do so, and so did I restart everything, but this time in Haskell. This led me here, to what I currently have right now, a fully functional basic compiler, mostly written by myself (except the kind checker, see [this pull request](https://github.com/Mesabloo/blob/pull/1)).

## <a name='Roadmap'></a>Roadmap

The ultimate goal is to provide these features:
- Basic features:
  - [x] Anonymous functions
  - [x] User defined operators with custom fixities
  - [x] Pattern matching
  - [x] Data types
  - [x] Type aliases
- Advanced features:
  - [x] Kinds
  - [x] GADTs
  - [ ] Type classes and instances
  - [ ] Quantitive types
  - [ ] Modules
- Advanced future features (not in the prototype):
  - [ ] Rank N types / `forall`
  - [ ] Type families (type-level functions)
  - [ ] Type applications
  - [ ] Laziness
  - [ ] Row polymorphism
  - [ ] Dependent types
  - [ ] Algebraic effects ?

## <a name='Unicode'></a>Unicode

Blob supports some unicode equivalent to ASCII characters:

| ASCII | Unicode |
|:-----:|:-------:|
| `->`  |   `→`   |
| `=>`  |   `⇒`   |
| `\`   |   `λ`   |
| `::`  |   `∷`   |
|`forall`|  `∀`   |

## <a name='Examples'></a>Examples

Some code examples can be found in the [examples](./examples) folder.

## <a name='Contributors'></a>Contributors

| User                               | What has been done                      |
|:------------------------------------------------:|:---------------------------------:|
| <img src="https://avatars.githubusercontent.com/felko" height=30px align=center> [`@felko`](https://github.com/felko) | Kind checker ; various help about many subjects (parsing, typechecking...)<br>Huge thanks for all the help given. |
| <img src="https://avatars.githubusercontent.com/mesabloo" height=30px align=center> [`@mesabloo`](https://github.com/mesabloo) | Compiler, REPL, and Interpreter |

## <a name='Referencesinspirations'></a>References, inspirations

* The [Granule project](https://github.com/granule-project/granule), a statically typed functional programming language with Graded Modal Types
* [Pikelet](https://github.com/pikelet-lang/pikelet), a dependently typed functional programming langage
* [GHC](https://github.com/ghc/ghc), the *Great* Haskell Compiler