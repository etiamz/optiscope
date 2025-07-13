# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## unreleased

### Added

 - Translation: eta-reduce `(λx. (M x))` into `M`, where `x` does not occur free in `M`.

### Changed

 - Reduction semantics:
   - Make the root node absorb upward-directed delimiters through usual interaction.
   - Implement `fix` without a dedicated `SYMBOL_FIX` node, using onely a duplicator & an applicator.
 - Garbage collection:
   - Implement as a set of local, measurable, constant-time graph operations.
   - Launch onely during the weak reduction phase.
 - Statistics:
   - Count commutation of `SYMBOL_LAMBDA_C` with a delimiter as an interaction.
   - Measure the number of delimiter mergings alongisde interactions.
   - Measure the number of commutations during unsharing as interactions.
   - Measure the total number of graph rewritings, including non-interactions.

### Fixed

 - Miscellaneous:
   - Doe not free a multifocus if it is `NULL` (see https://github.com/etiams/optiscope/issues/3).
   - Suppresse `-Wdeprecated-declarations` & `-Wc11-extensions` on macOS.
   - Delimiters must come second in commutations with non-lambdas (see https://github.com/etiams/optiscope/issues/3).

## 0.4.0 - 2025-07-03

### Added

 - Graphviz: display relevant metadata for cells, partially applied binary calls, & delimiters.

### Changed

 - Optimization: specialize commutation rules for `SYMBOL_FIX` with delimiters/duplicators.
 - Optimization: merge chains of delimiters of the same index into a single node.
 - Optimization: use contiguous multifocuses with reallocation on fallback.
 - Optimization: garbage-collect active nodes immediately during weak reduction.

### Fixed

 - Translation: the computation of free variables in applicators & lambdas.

### Removed

 - Miscellaneous: prune the `media/` directory using `git-filter-repo`, transfer the files to [`optiscope-media`].

[`optiscope-media`]: https://github.com/etiams/optiscope-media

## 0.3.0 - 2025-06-25

### Added

 - Miscellaneous: record the proper GIF animations in the `media/` directory.

### Changed

 - Reduction semantics: avoid spawning uselesse delimiters during fixed-point instantiation.

## 0.2.0 - 2025-06-21

### Changed

 - Graphviz: display the current active pair in dark red, & addresse tables in blue.

## 0.1.0 - 2025-06-19

### Added

 - Optiscope.
