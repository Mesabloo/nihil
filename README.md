# Nihil

<img src="./assets/icon.png" alt="Nihil icon" align=right width=128px />

[![license](https://img.shields.io/github/license/mesabloo/nihil?style=for-the-badge)](./LICENSE)

[![stars](https://img.shields.io/github/stars/mesabloo/nihil?color=%23fdaa33&style=for-the-badge)](https://github.com/mesabloo/nihil/stargazers)    [![forks](https://img.shields.io/github/forks/mesabloo/nihil?color=%23654321&label=Forks&style=for-the-badge)](https://github.com/mesabloo/nihil/network/members)

## <a name='Tableofcontents'></a>Table of contents

<!-- Automatically generated table of contents -->
*  [Table of contents](#Tableofcontents)
*  [About Nihil](#Aboutnihil)
*  [Why Nihil?](#Whynihil)
*  [Roadmap](#Roadmap)
*  [Unicode](#Unicode)
*  [Examples](#Examples)
*  [Contributors](#Contributors)
*  [References, inspirations](#Referencesinspirations)

## <a name='Aboutnihil'></a>About Nihil

[![programming language](https://img.shields.io/github/languages/top/mesabloo/nihil?color=%20%235e5086&style=for-the-badge)](https://github.com/Mesabloo/nihil/search?l=haskell)    [![code size](https://img.shields.io/github/languages/code-size/mesabloo/nihil?color=%23123456&style=for-the-badge)](./)

Nihil is a statically- and quantitively-typed functional programming language aimed at producing type- and data-safe lazy programs running without Garbage Collectors.

It is more in a “proof of concept” stage than production-ready, but a prototype will soon appear as there aren't much things left to do.
(well there is in terms of difficulty, but not in terms of number)

## <a name='Whynihil'></a>Why Nihil?

Nihil came to life after I had an idea: *“what if we could control how much resources are used?”*

In the beginning, I was willing to make an esoteric language, poorly designed, but functional.

Beginning with some C++, I built my own parser combinators library, an old crappy one, which in the end wasn't working that well, because of some strange things I am still unable to really understand.

[@felko](https://github.com/felko) introduced me to Haskell, and I wanted to give it a try for many months. That was the perfect time to do so, and so did I restart everything, but this time in Haskell. This led me here, to what I currently have right now, a fully functional basic compiler, mostly written by myself (except the old kind checker, see [this pull request](https://github.com/Mesabloo/nihil/pull/1)).

[![builtwithheart](https://forthebadge.com/images/badges/built-with-love.svg)](./)

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
  - [x] Equational pattern matching
  - [ ] Type classes and instances
  - [ ] Quantitive types
  - [ ] Modules
  - [ ] Polymorphic kinds
- Advanced future features (not in the prototype):
  - [ ] Rank N types / `forall`
  - [ ] Type families (type-level functions)
  - [ ] Type applications
  - [ ] Laziness
  - [ ] Row polymorphism
  - [ ] Dependent types
  - [ ] Algebraic effects ?

## <a name='Unicode'></a>Unicode

Nihil natively supports some unicode equivalents to ASCII characters:

| ASCII | Unicode |
|:-----:|:-------:|
| `->`  |   `→`   |
| `=>`  |   `⇒`   |
| `\`   |   `λ`   |
|`forall`|  `∀`   |

## <a name='Examples'></a>Examples

Some code examples can be found in the [examples](./examples) folder.

## <a name='Contributors'></a>Contributors

| User                               | What has been done                      |
|:------------------------------------------------:|:---------------------------------:|
| <img src="https://avatars.githubusercontent.com/felko" height=30px align=center> [`@felko`](https://github.com/felko) | Kind checker ; various help about many subjects (parsing, typechecking...)<br>Huge thanks for all the help given. |
| <img src="https://avatars.githubusercontent.com/mesabloo" height=30px align=center> [`@mesabloo`](https://github.com/mesabloo) | Basically everything else |

## <a name='Referencesinspirations'></a>References, inspirations

* The [Granule project](https://github.com/granule-project/granule), a statically typed functional programming language with Graded Modal Types
* [Pikelet](https://github.com/pikelet-lang/pikelet), a dependently typed functional programming language
* [GHC](https://github.com/ghc/ghc), the *Great* ~~Glasgow~~ Haskell Compiler
