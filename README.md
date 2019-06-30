# ghc-typelits-extra

[![Build Status](https://secure.travis-ci.org/clash-lang/ghc-typelits-appendsymbol.svg?branch=master)](http://travis-ci.org/clash-lang/ghc-typelits-appendsymbol)
[![Hackage](https://img.shields.io/hackage/v/ghc-typelits-appendsymbol.svg)](https://hackage.haskell.org/package/ghc-typelits-appendsymbol)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/ghc-typelits-appendsymbol.svg?style=flat)](http://packdeps.haskellers.com/feed?needle=exact%3Aghc-typelits-appendsymbol)

A GHC constraint-solver plugin that witnesses the injectivity of
`GHC.TypeLits.AppendSymbol`. To use the plugin, add the

```
OPTIONS_GHC -fplugin GHC.TypeLits.AppendSymbol
```

pragma to the header of your file.
