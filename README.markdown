# Autoexporter

[![Workflow](https://github.com/tfausak/autoexporter/actions/workflows/ci.yml/badge.svg)](https://github.com/tfausak/autoexporter/actions/workflows/ci.yml)
[![Hackage](https://badgen.net/hackage/v/autoexporter)](https://hackage.haskell.org/package/autoexporter)

Autoexporter automatically re-exports Haskell modules.

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

That will generate code that looks like this:

``` haskell
module M (
  module M.A,
  module M.B,
) where
import M.A
import M.B
```

Autoexporter will generally behave as you'd expect, but there are a couple
things to look out for:

- You cannot selectively include or exclude any modules.

- By default, only immediate children will be re-exported. If you use this in
  some module `M`, it won't pull in `M.A.B`. If you need deep re-exporting,
  please pass `--deep` to Autoexporter like this:

  ```haskell
  {-# OPTIONS_GHC -F -pgmF autoexporter -optF --deep #-}
  ```
