---
title: ghc-mod ELF 42 with yesod scaffolded site
---

## Problem
Recently, as I started working with Yesod's scaffolded site, `ghc-mod` didn't work.
It reports an error like this:
```
ghc-mod: /home/zelinf/.stack/snapshots/x86_64-linux/lts-9.2/8.0.2/lib/x86_64-linux-ghc-8.0.2/yaml-0.8.23.3-1Jrp8dCoYxM4ztwxVbJIUL/libHSyaml-0.8.23.3-1Jrp8dCoYxM4ztwxVbJIUL.a: unhandled ELF relocation(RelA) type 42

ghc-mod: Could not on-demand load symbol 'simple_document_start'

ghc-mod: /home/zelinf/.stack/snapshots/x86_64-linux/lts-9.2/8.0.2/lib/x86_64-linux-ghc-8.0.2/yaml-0.8.23.3-1Jrp8dCoYxM4ztwxVbJIUL/libHSyaml-0.8.23.3-1Jrp8dCoYxM4ztwxVbJIUL.a: unknown symbol `simple_document_start'
ghc-mod: Could not on-demand load symbol 'yamlzm0zi8zi23zi3zm1Jrp8dCoYxM4zztwxVbJIUL_TextziLibyaml_zdfShowEvent_closure'

ghc-mod: /home/zelinf/.stack/snapshots/x86_64-linux/lts-9.2/8.0.2/lib/x86_64-linux-ghc-8.0.2/yaml-0.8.23.3-1Jrp8dCoYxM4ztwxVbJIUL/libHSyaml-0.8.23.3-1Jrp8dCoYxM4ztwxVbJIUL.a: unknown symbol `yamlzm0zi8zi23zi3zm1Jrp8dCoYxM4zztwxVbJIUL_TextziLibyaml_zdfShowEvent_closure'
ghc-mod: Could not on-demand load symbol 'yamlzm0zi8zi23zi3zm1Jrp8dCoYxM4zztwxVbJIUL_DataziYamlziInternal_zdfExceptionParseException_closure'

ghc-mod: .stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/Settings.o: unknown symbol `yamlzm0zi8zi23zi3zm1Jrp8dCoYxM4zztwxVbJIUL_DataziYamlziInternal_zdfExceptionParseException_closure'
```
`binutils-2.26` (or some newer version) uses a new ELF relocation type, which ghc-mod can't identify.

## Solution
One possible workaround is to compile packages that suffer this problem with ghc option `-opta-Wa,-mrelax-relocations=no` (no spaces after the comma). Detailed steps:
1. Identify which package suffers. From the error message above, we see it's `yaml-0.8.23.3` in this case. (But it might be other packages)
2. Add
```yaml
ghc-options:
  yaml: -opta-Wa,-mrelax-relocations=no
```
to your `stack.yaml`.
The options will be passed to ghc while recompiling `yaml`
3. Force a recompilation
```
stack exec -- ghc-pkg unregister yaml --force
stack build --force-dirty
```
It should work fine now.

---

References:
[discussion](https://github.com/DanielG/ghc-mod/issues/762)
[workaround](https://github.com/gibiansky/IHaskell/issues/636)
