Description
===========

Abstract way of building applications that have multiple modes of operation and
configuration passed on command line and/or using configuration files.

Intention is to provide logical separation of action and configuration. Under
action we understand what application will do after command line options are
processed, configuration files loaded and environment variables checked. The
result of all this we call (program or application) configuration.

Action has to have a Semigroup instance that provides notion how actions may
change. Both, action and configuration, has to have Default instances. These
together define default behaviour of an application.

For small applications this might be a little overhead. Other packages may be
better for that. For more complicated and simply extensible applications this
library provides very consistent way how to extend it. Adding new action is
simple, changing default mode of operation is also very simple. Since action
and configuration are separated changing one doesn't automatically mean that
the other will have to be changed also.


External Dependencies
=====================

* [*comonad*][comonad] -- Haskell 98 compatible comonads.
* [*data-default-class*][data-default-class] -- A class for types with a
  default value.
* [*not-found*][not-found] -- Utility library.
* [*pretty*][pretty] -- Pretty-printing library.
* [*semigroups*][semigroups] -- Haskell 98 semigroups.
* [*terminal-size*][terminal-size] -- Get terminal window height and width.


Building options
================

* `-fpedantic` (disabled by default)
  Pass additional warning flags including `-Werror` to GHC during compilation.


Contributions
=============

Pull requests, bug reports and generally contributions in any form are welcome!
Please don't be afraid to contact author using GitHub or by e-mail (see
`.cabal` file for that).

Also try to use `-fpedantic` flag during development and testing.


Lincense
========

This package is under BSD3 license, see `LICENSE` file for details.

All dependencies are also under the same license. See individual packages on
hackage for details:

* [base][]
* [comonad][]
* [data-default-class][]
* [not-found][]
* [pretty][]
* [semigroups][]
* [terminal-size][]


[base]:
  http://hackage.haskell.org/package/base/
  "HackageDB: base"

[comonad]:
  http://hackage.haskell.org/package/comonad/
  "HackageDB: comonad"

[data-default-class]:
  http://hackage.haskell.org/package/data-default-class/
  "HackageDB: data-default-class"

[not-found]:
  https://github.com/trskop/hs-not-found
  "GitHub: trskop/hs-not-found"

[pretty]:
  http://hackage.haskell.org/package/pretty/
  "HackageDB: pretty"

[semigroups]:
  http://hackage.haskell.org/package/semigroups/
  "HackageDB: semigroups"

[terminal-size]:
  http://hackage.haskell.org/package/terminal-size/
  "HackageDB: terminal-size"
