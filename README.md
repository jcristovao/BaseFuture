BaseFuture
==========

GHC current development branch, 7.7 (soon 7.8) adds some new features that are perfectly retroactive into older versions.

By carefuly selecting those, mainly new functions and additional typeclass instances, this library allows you to
use some of those functions right now without upgrading your compiler. The reasons one could wish for this are several,
namely:

* Other packages that you require still do not compile in GHC Head,
* You are bounded by company policy to an older version of GHC and/or the Haskell Platform,
* You just wish to try out some of the new functions, without the hassle of installing a new compiler toolchain.

Please note that these are very few functions. I've done this mainly to have proper ```Foldable``` instances for Either,
among other small things.

This library will just be a NOP under GHC 7.7/7.8, so your code will be forward compatible.

Usage
-----

Add this to your cabal file:

```
BaseFuture   >= 0.1
```

And then replace any of the modified modules like this, instead of:

```
import Data.Either
```

Try:

```
import Data.Either.Compat
```

Each of these modules re-exports the installed version of the module, plus the added functions or instances.


You are most welcome to contribute. The rules are just common-sense, I guess:

* Include only stuff with no dependencies on new compiler features
* Focus mainly on additional typeclasses instances
* Or new functions, that otherwise should not interfere with existing code.
* Use CPP to translate the code into NOP when the new compiler finally enters the Haskell Platform.

As a final note, all this code _is_ from the GHC code base. I take absolutely no credit for it, I just re-packaged it.
