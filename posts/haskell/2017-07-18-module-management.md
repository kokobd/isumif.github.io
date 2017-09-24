---
title: Haskell Module Management
---

For a haskell library managed with stack, some functions should be accessible from test suite, but not from the user. By looing around on the Internet, I found 2 ways.

## Use 'Internal' submodules
Suppose we have a module `Zelinf.Lib`. Then we create a submodule `Zelinf.Internal.LibImpl` and put all functions we need in `LibImpl`. `LibImpl` exports every function it defines. `Lib` imports some of `LibImpl`'s functions, and re-exports them. In the package description file, we export both modules.

This allows the user to *hack* our library, if they need, with modules in `Internal`.

## Use the C Pre-processor
Enable the language extension `CPP` and use `#ifdef TEST` to conditionally exports *private* functions.
I haven't really tried this way, though it seems nice.
