---
title: Build latex with Shake
---

[Shake][1] is a powerful build system written in Haskell. Below is a basic build
file to compile latex documents.

```haskell
#!/usr/bin/env stack
{- stack
  script
  --resolver lts-13.12
  --package shake
-}

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

buildDir :: FilePath
buildDir = "build"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=buildDir} $ do
    want [buildDir </> "report.pdf"]

    phony "clean" $ do
      removeFilesAfter "build" ["//*"]

    buildDir </> "report.pdf" %> \out -> do
      texFiles <- getDirectoryFiles "" ["//*.tex"]
      need texFiles
      images <- getDirectoryFiles "" ["images//*.png"]
      need images
      cmd_ "latexmk -pdf -bibtex -xelatex -interaction=nonstopmode"
        ("-outdir=" ++ buildDir)
        "-shell-escape"
```

[1]: https://shakebuild.com/