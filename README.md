# [Autoexporter][]

Autoexporter automatically re-exports Haskell modules.

[![Version badge][]][version]
[![Build badge]][build]

-   [Install](#install)
-   [Use](#use)

## Install

1.  Install [Stack][].

2.  `stack install autoexporter`

## Use

Let's say you have a module `M` that just exports some other modules. It might
look like this:

``` haskell
module M
    ( module M.A
    , module M.B
    ) where

import M.A
import M.B
```

This code is error-prone. If you add a new module, say `M.C`, you have to
remember to come back to this file and re-export it. And this code is tedious
to write. You have to list each module twice. You can do a little better, but
not much.

``` haskell
module M (module X) where
import M.A as X
import M.B as X
```

Now you don't have to write every module twice, but you still have to remember
to re-export everything. And the generated documentation for this module
doesn't include anything about the exported modules.

Autoexporter handles this for you. Instead of either of the above approaches,
simply drop this into the `M` module:

``` haskell
{-# OPTIONS_GHC -F -pgmF autoexporter #-}
```

That will generate code like the first example. A couple caveats:

-   Your source files must be rooted in a directory called `library`.

    -   I will happily accept patches that lift this restriction. This is good
        enough for my purposes now, but it's pretty bad in general. The hard
        part of this problem is knowing when to stop when converting a file
        path into a module name. For example, how can we reliably convert
        `/home/taylor/HSPackage/Data/Package.hs` into `Data.Package`?

-   Absolutely nothing else can be in the source file. Autoexporter will blow
    up if it finds anything else.

-   Only immediate children will be re-exported. If you use this in some module
    `M`, it won't pull in `M.A.B`.

-   You cannot selectively leave out any modules. You also cannot selectively
    exclude any imports from any of the modules.

    -   This could be allowed via `-optF`. I will happily accept patches for
        this as well.

[Autoexporter]: https://github.com/tfausak/autoexporter
[Version badge]: https://img.shields.io/hackage/v/autoexporter.svg?label=version
[version]: https://hackage.haskell.org/package/autoexporter
[Build badge]: https://travis-ci.org/tfausak/autoexporter.svg?branch=master
[build]: https://travis-ci.org/tfausak/autoexporter
[Stack]: http://haskellstack.org
