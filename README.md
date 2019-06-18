# optica

This repository contains *Optica*, the library associated to the article
*Viewing Language-Integrated Query From a Different Optic* (J. López González
and Juan M.  Serrano), which was recently submitted to *Science of Computer
Programming*.

## Repo Structure

### Core

- [concrete](core/src/main/scala/optica/concrete): definition of read-only
  concrete optics.
- [symantics](core/src/main/scala/optica/symantics): symantics of Optica (both
  optic combinators and actions), along with the standard, XQuery, TripletFun
  and T-LINQ interpretations.
- [xquery](core/src/main/scala/optica/xquery): algebraic data type that
  represents a subset of the XQuery grammar, with an interpretation to textual
  XQuery expressions.
- [triplet](core/src/main/scala/optica/triplet): definition of `TripletFun` and
  involved data types, with an interpretation to SQL select statements.
- [sql](core/src/main/scala/optica/sql): algebraic data type that represents a
  subset of the SQL grammar, with an interpretation to textual SELECT
  statements.
- [tlinq](core/src/main/scala/optica/tlinq): subset of T-LINQ primitives, along
  with the standard semantics.

### Examples

- [couple](example/src/main/scala/example/couple): couple model and general
  queries over it.
- [organisation](example/src/main/scala/example/org): organisation model and
  general queries over it.
- [test](example/src/test): suite of tests to execute general queries over
  different underlying infrastructures.

## Running Tests

1. Install [SBT](https://www.scala-sbt.org/)
2. Clone this repo: `$ git clone https://github.com/hablapps/scp19.git`
3. Change directory: `$ cd scp19`
4. Run tests: `$ sbt test` (it might take up to several minutes)

