---
title: Haskell Programming Environment in 2018
---

Stack and Cabal
===============
*Cabal* is Haskell's package manager. However, due to complex dependencies beween packages, Cabal might not be able to
resolve these issues. Then *Stack* comes to rescue, which maintains consistent snapshots of Cabal packages. One can
compare cabal to pip and stack to anaconda.

Stack can either be used with an exsiting cabal project or use its own yaml config file. To create a stack project on
top of cabal, use ``stack init``. To create a new stack project, use ``stack new project_name``. Stack provides handy
shortcuts: build, repl, ghci, exec, test etc.

Testing
=======
I use *HUnit* for unit tests, *QuickCheck* for property checks. I also use *tasty* to structure the test cases in a
hierarchical manner and selectively execute test cases.

Haskell in Vim
==============
Actually Vim does not have good support for Haskell type inference/checking (Emacs does a better job here). I'm
currently use ALE plugin and hlint to do async linting. This is far from perfect, but works minimally. AFAIK, most
existing Vim Haskell plugins are fragile and easy to break. Maybe Langugue Server Protocol can fill this gap, but
those projects are still WIP.

