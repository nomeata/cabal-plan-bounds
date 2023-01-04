cabal-plan-bounds: generate cabal bounds from actual build plans
================================================================

TL;DR: Updates the `.cabal` file’s `build-depends` and (not yet) `tested-with` based on the
`build.json` of actual build paths. You never have to edit the build depends
manually again.

The problem
-----------

Manually curated dependency version ranges tend to become a lie: They likely
include versions of your the dependencies that are neither longer tested by your CI
system, or implied by compatibility with the tested versions (by way of the [PVP]).

Typically, these are versions near the lower edge of the bounds, but can also
be on the upper end (e.g. when they are packaged with GHC and Cabal prefers installed versions, or when they are not actually installable yet).

There are ways to mitigate this problem, such as being very careful, and maybe
using Cabals new [`--prefer-oldest`] flag. But these are not reliable.

[PVP]: https://pvp.haskell.org/
[`--prefer-oldest`]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-prefer-oldest

The solution
------------

So the conclusion must be to **not write build-depends ranges by hand.**
Which is an unpleaseant chore instead.

Instead, **derive the build-depends from your actual CI builds**.

Presumably you test your code in different situations anyways – different
versions of GHC, stackage releases etc. Keep doing that, collect the _actual_
build plans used in these CI systems. Then pass them to `cabal-plan-bounds` which
will update the bounds accordingly.

Simple example
--------------

For a simple example, you can just call `cabal build` with different compilers:


    $ cabal build -w ghc-8.10.7 --builddir dist-8.10.7
    $ cabal build -w ghc-9.0.2 --builddir dist-9.0.2
    $ cabal build -w ghc-9.2.5 --builddir dist-9.2.5
    $ cabal build -w ghc-9.4.4 --builddir dist-9.4.4

and then update the cabal file

    $ cabal-plan-bounds dist-{8.10.7,9.0.2,9.2.5,9.4.4}/cache/plan.json -c cabal-plan-bounds.cabal

This will lead to the following diff:

```diff
diff --git a/cabal-plan-bounds.cabal b/cabal-plan-bounds.cabal
index 1db21ca..a99e7bc 100644
--- a/cabal-plan-bounds.cabal
+++ b/cabal-plan-bounds.cabal
@@ -20,9 +20,12 @@ executable cabal-plan-bounds
     import:           warnings
     main-is:          Main.hs
     other-modules:    ReplaceDependencies
-    build-depends:    base, Cabal-syntax, cabal-plan,
-                      optparse-applicative, containers,
-                      text
-    build-depends:    bytestring,
+    build-depends:    base ^>=4.14.3.0 || ^>=4.15.1.0 || ^>=4.16.4.0 || ^>=4.17.0.0,
+                      Cabal-syntax ^>=3.8.1.0,
+                      cabal-plan ^>=0.7.2.3,
+                      optparse-applicative ^>=0.17.0.0,
+                      containers ^>=0.6.4.1,
+                      text ^>=1.2.4.1 || ^>=2.0.1
+    build-depends:    bytestring ^>=0.10.12.0 || ^>=0.11.3.1
     hs-source-dirs:   src/
     default-language: Haskell2010
```

More sophisticated setup
------------------------

For a more sophisticated setup, you can create multiple `cabal.project` files,
one for each setting:

```
$ ls ci-configs/
ghc-8.10.7.config  ghc-9.2.5.config  stackage-nightly.config
ghc-9.0.2.config   ghc-9.4.4.config
$ cat ci-configs/ghc-9.4.4.config
import: cabal.project
active-repositories: hackage.haskell.org:merge
index-state: hackage.haskell.org 2022-12-21T10:40:48Z
with-compiler: ghc-9.4.4
```

Here we pin the compiler version and the precise view of Hackage repo to get
reproducible results. You can imagine a separate tool that regularly updates these time stamps.

Similarly, we can pull in stackage configurations, simply by importing the
corresponding `cabal.config`, which also pins down the compiler

```
$ cat ci-configs/stackage-nightly.config
import: cabal.project
import: https://www.stackage.org/nightly-2023-01-03/cabal.config
```

(TODO: Probably these should also pin the `index-state` for reproducibility.)

Now you can configure your CI system to run one job for each of these configs,
collect the `plan.json` files, and finally check that the version bounds in your
`.cabal` file match, and if not, complain or auto-update them.

Usage
-----

```
cabal-plan-bounds -- --help
Derives dependency bounds from build plans

Usage: cabal-plan-bounds [PLAN] [-c|--cabal CABALFILE]

Available options:
  -h,--help                Show this help text
  PLAN                     plan file to read (.json)
  -c,--cabal CABALFILE     cabal file to pdate (.cabal)
```

Features and limitations
------------------------

* It edits the `.cabal` file in place

* It leaves the `.cabal` file as is: No reformatting, all comments are preserved.

* Only the `build-dependens` fiels are touched. They are reformatted (one dependency per line).

  It does not add, remove or reorder the packages mentioned in the dependencies.

* It will apply the same bounds to _all_ mentions of a dependency in the
  `.cabal` file (e.g. in different components, or in conditionals).

  It does not support different ranges in different components. (Maybe it could
  be smarter here, but due to `common` sections and conditionals, it cannot be
  complete.)

  This means some behaviour cannot be achieved. Maybe this needs to be revised
  (especially with regard to conditionals).

Future work (contributions welcome!)
------------------------------------

* Proper error handling, e.g. while parsing.
* A test suite
* Printing a nice human readable summary of dependency changes.
* A `--dry-run` mode that does not touch the `.cabal` file.
* A `--check` mode that does not touch the `.cabal` file, but fails if it would
  change it (for CI).
* Update the `tested-with` field accordig to the compiler versions used.
