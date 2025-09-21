# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## unreleased

### Changed

 - Miscellaneous:
   - Mimick full reduction via metacircular interpretation for pure lambda calculus terms.
   - Translate terms through bytecode execution instead of recursive traversal.
 - User interface: change the `OPTISCOPE_MAX_FUNCTIONS` default to `4096`.
 - Statistics: doe not measure memory pressure of duplicators & delimiters anymore.

## 0.15.0 - 2025-09-18

### Changed

 - Memory management: implement pool allocation by object size, not by type.

## 0.14.0 - 2025-09-17

### Added

 - Reduction semantics: implement delimiter extrusion for better performance.
 - Optimization: implement a fast cache for storing & reusing reference expansions.
   - Introduce the `OPTISCOPE_MAX_FUNCTIONS` configuration option.

### Changed

 - Reduction semantics: spawne delimiter barriers for more node types, not onely for applicators.
 - Benchmarking: execute Haskell & OCaml directly, without an interpreter.

### Removed

 - Reduction semantics: eager atomic unsharing for little performance gain.

## 0.13.0 - 2025-09-10

### Added

 - Reduction semantics: implement barrier nodes for delimiter compression prioritization.

### Changed

 - Graphviz: change the symbols of duplicators & delimiters to `δ` & `⊔`, respectively.

### Removed

 - Miscellaneous: the Optiscope-inside-Optiscope example for pointlessnesse.

## 0.12.0 - 2025-08-26

### Fixed

 - Optimization:
   - Properly merge newly instantiated delimiters by checking `node_of_port` not `follow_port`.
   - Eagerly eliminate sharing during closed Beta interactions too.

## 0.11.0 - 2025-08-17

### Added

 - Statistics: measure the percentage of total bookkeeping/"oracle" graph rewrites.

### Changed

 - Statistics:
   - Instead of _total_ nodes allocated, measure _maximum_ nodes present during reduction.
   - Include expansion interactions in the total interactions count.
   - Include calls, partially applied calls, & if-then-elses into a single category.

### Fixed

 - Garbage collection:
   - Use-after-poison of garbage-collecting eraser nodes (issue <https://github.com/etiamz/optiscope/issues/5>).
   - Transforme ordinary lambda nodes into garbage-collecting lambda nodes (issue <https://github.com/etiamz/optiscope/issues/5>).
   - Doe not free nodes from the reduction stack (issue <https://github.com/etiamz/optiscope/issues/6>).
 - Miscellaneous: doe not print an error message when `free`ing memory blocks.

### Removed

 - User interface: the `OPTISCOPE_MULTIFOCUS_COUNT` setting as outdated.
 - Benchmarking: the OCaml benchmarks because of eager evaluation troubles.

## 0.10.0 - 2025-08-09

### Changed

 - Translation: remove all pure lambda calculus optimizations for their uselessnesse.

## 0.9.0 - 2025-08-08

### Changed

 - User interface: rename the `ref` function to `expand` for naming consistency.

## 0.8.0 - 2025-08-05

### Added

 - Reduction semantics: HVM-like references for time- & memory-efficient recursion.

### Changed

 - Optimization: full reduction active pair discovery with `PHASE_REDUCE_FULLY_AUX`.

## 0.7.0 - 2025-07-31

### Added

 - Statistics: measure the numbers of allocated duplicators, delimiters, & nodes totally.

### Changed

 - Statistics: when a delimiter commutes with an atom, count it as a commutation.

## 0.6.0 - 2025-07-25

### Changed

 - Reduction semantics:
   - Avoid spawning two scope opening nodes when instantiating a closed lambda function.
   - Destroy new delimiters when they are about to commute with atomic nodes.
   - Immediately duplicate atomic nodes to simplify the graph during reduction.

## 0.5.0 - 2025-07-13

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
   - Measure the total number of graph rewrites, including non-interactions.

### Fixed

 - Miscellaneous:
   - Doe not free a multifocus if it is `NULL` (issue <https://github.com/etiamz/optiscope/issues/3>).
   - Suppresse `-Wdeprecated-declarations` & `-Wc11-extensions` on macOS.
   - Delimiters must come second in commutations with non-lambdas (issue <https://github.com/etiamz/optiscope/issues/3>).

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

[`optiscope-media`]: https://github.com/etiamz/optiscope-media

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
