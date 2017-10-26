---
title: Introduction to Full Stack Haskell Web Development
---

Here we introduce a haskell full-stack web development solution.

Our servers do not directly generate HTML pages. Instead, they provide
JSON APIs. Client code should call those APIs to acheive functionality.
In web pages, JS code render the pages using server-provided APIs. This
is one of the easiest way to build highly interactive web pages.

## GHCJS
JavaScript has too many problems. Thanks to GHCJS, we can write in haskell,
then compile it down to JavaScript. We will use `stack` in this tutorial.

Client-side code should have its own stack.yaml. Unlike common applications
and libraries, we don't have a `stack.yaml` at project root. We need at least
3 sub directories:

- client - contains a `stack.yaml`
- common - a cabal package, referenced in both client and server
- server - contains a `stack.yaml`

Now let's focus on client-side code.

1. Install a recent version of Node.js
2. `stack new` or directly create the project by hand. We need to tell stack
   to [use GHSJS over regular GHC](https://docs.haskellstack.org/en/stable/ghcjs/)
3. Run `stack setup`. It will take 30 minutes or more, as `stack` compiles
GHCJS from source in this stage.
4. Add `miso` as a dependency in cabal file, copy [Sample Application](https://github.com/dmjio/miso#sample-application) into your
`Main.hs`
5. Try `stack build`, then you probably need to use `stack solver` to add `miso`
   to your `extra-deps`. A few JS files will be generated in `$(stack path --local-install-root)/bin/EXECUTABLE.jsexe/all.js`, where `EXECUTABLE` is
   the executable name configured in your cabal file.
6. Open `index.html` in your browser, you should see a working client-side application.

During development, we prefer GHCJSi, an interactive environment like GHCi.

1. Add `maini = clearBody >> main` to your `Main.hs`. Import `Miso.Dev` if you
didn't.
2. Run `npm install socket.io` in your `client` project root. `socket.io` is
   needed by GHCJSi to interact with the browser.
3. Run `export NODE_PATH=$(pwd)/node_modules`
4. Use `stack ghci` to start GHCJSi. You should see something like `socket.io found, browser session available at http://localhost:6400` in your GHCJSi session.
5. Open a browser, navigate to `http://localhost:6400`
6. Run `maini` in GHCJSi, you should see your application in the browser.

**IMPORTANT:** You must open the browser and navigate to that web page **BEFORE**
running `maini` in GHCJSi. Otherwise, strange error message may arise.
