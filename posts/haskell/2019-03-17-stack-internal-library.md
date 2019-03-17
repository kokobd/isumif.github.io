---
title: Stack error on internal libraries
---

If your dependencies include `typerep-map`, and you are on Stackage lts-13.*,
you may encounter the following error message when running `stack haddock`

```
Run from: ***
Standard output:

haddock: internal error: ./z-typerep-map-z-typerep-extra-impls-0.3.1/z-typerep-map-z-typerep-extra-impls-0.3.1.haddock openBinaryFile: does not exist (No such file or directory)
```

The lastest release of stack is 1.9.3, which doesn't support haddocking internal
libraries (a new feature of Cabal 2.0).

However, the problem is already [fixed][1] in stack's master branch, so we may
upgrade stack.
```
stack upgrade --git
```

If you don't want to upgrade, there is a workaround:

1. Go to the directory mentioned as "Run from" in the error message
2. There you will find directory `z-typerep-map-z-typerep-extra-impls-0.3.1`
3. Inside that directory, rename the only .haddock file to
   `z-typerep-map-z-typerep-extra-impls.haddock`

Then try `stack haddock` or `stack hoogle --rebuild` again. The problem is gone.
You may receive warning about `z-typerep-map-z-typerep-extra-impls`'s
documentation is missing. Just ignore it.

[1]: https://github.com/commercialhaskell/stack/pull/4596