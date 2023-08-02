# Weeder

Weeder is an application to perform whole-program dead-code analysis. Dead code
is code that is written, but never reachable from any other code. Over the
lifetime of a project, this happens as code is added and removed, and leftover
code is never cleaned up. While GHC has warnings to detect dead code is a single
module, these warnings don't extend across module boundaries - this is where
Weeder comes in.

Weeder uses HIE files produced by GHC - these files can be thought of as source
code that has been enhanced by GHC, adding full symbol resolution and type
information. Weeder builds a dependency graph from these files to understand how
code interacts. Once all analysis is done, Weeder performs a traversal of this
graph from a set of roots (e.g., your `main` function), and determines which
code is reachable and which code is dead.

# Using Weeder

## Preparing Your Code for Weeder

To use Weeder, you will need to generate `.hie` files from your source code.

### Cabal

If you use Cabal, this is easily done by adding one line to your
`cabal.project.local` file:

``` cabal
package *
  ghc-options: -fwrite-ide-info
```

Once this has been added, perform a full rebuild of your project:

``` shell
cabal clean
cabal build all
```

### Stack

If you use `stack`, add the following to your `stack.yaml`:

``` yaml
ghc-options:
  "$locals": -fwrite-ide-info
```

and rebuild:

``` shell
stack clean
stack build
```

## Calling Weeder

To call Weeder, you first need to provide a configuration file, `weeder.toml`. Weeder uses
[TOML](https://toml.io/en/) as its configuration format.

`roots` is a list of regular expressions of symbols that are considered as
alive. If you're building an executable, the pattern `^Main.main$` is a
good starting point - specifying that `main` is a root. Weeder currently doesn't
add all exported functions as roots automatically but in many cases `main` from a
test suite could be a good workaround for that

`type-class-roots` configures whether or not Weeder should consider all instances
of type classes as roots. Defaults to `false`.

``` toml
roots = [ "^Main.main$" ]
type-class-roots = true
```

Now invoke the `weeder` executable, and - if your project has weeds - you will
see something like the following:

``` shell
$ weeder
src/Dhall/TH.hs:187: toDeclaration
src/Dhall/TH.hs:196: toNestedHaskellType
```

… which indicates the location of two unused symbols.
(Please note these warnings are just for demonstration and not necessarily weeds
in the Dhall project).

# Tips

- You may want to add `^Paths_.*` to the roots in `weeder.toml` to ignore the
  `Paths_packageName` module automatically generated by Cabal.

# Limitations

Weeder currently has a few limitations:

## Overloaded syntax

On some versions of GHC, Weeder might report various type classes that are used
for syntax extensions as weeds. For example, `Num` and `IsString` classes might be
flagged as weeds if they are only used for overloaded literal syntax (that is,
the `fromInteger` and `fromString` methods).

You can add instances of specific type classes as roots with the `root-classes` 
field, or toggle whether Weeder considers all type class instances as roots with 
the `type-class-roots` configuration option.

## Template Haskell

Weeder is currently unable to parse the result of a Template Haskell splice. If
some Template Haskell code refers to other source code, this dependency won't be
tracked by Weeder, and thus Weeder might end up with false positives.
