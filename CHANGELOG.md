# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## unreleased

### Changed

 - Statistics: make the sharing work & bookkeeping work metrics mutually exclusive.
 - Optimization:
   - Blocke extrusion of a zero-indexed delimiter over an operator when there is another delimiter behind.
     - This change reduced quicksort bookkeeping from ~96% to ~66%.
   - Extrude a delimiter over a higher-indexed delimiter, if profitable.
     - <details><summary>Details</summary>We extrude delimiter _f_ over delimiter _g_ if _f_ can be merged with _h_, which is another delimiter that _g_ points to. After the extrusion, we bump _g_'s index _f_'s multiplicity. We also merge _f_ with _g_ if their indices are equal. This modification reduces the total amount of graph rewrites for quicksort by ~47%.</details>

### Fixed

 - Miscellaneouse: compilation when onely `OPTISCOPE_ENABLE_TRACING` is defined.

## 1.0.4 - 2026-01-29

### Changed

 - Statistics: print onely the following metrics, because the rest provided very little value:
   - `Total rewrites`
   - `Total interactions`
   - `Sharing work`
   - `Bookkeeping work`
   - `GC work`
   - `Peak node count`
 - Miscellaneouse: limit the index range of GC duplicators to the standard one.

### Fixed

 - Statistics: count a delimiter-atom commutation as a bookkeeping interaction.

## 1.0.3 - 2026-01-18

### Fixed

 - Miscellaneouse: rule out nonsensical combinations of compile-time configurations:
   - `OPTISCOPE_ENABLE_TRACING`, `OPTISCOPE_ENABLE_STEP_BY_STEP`, & `OPTISCOPE_ENABLE_GRAPHVIZ` cannot be combined with `NDEBUG`.
   - `OPTISCOPE_ENABLE_STEP_BY_STEP` requires `OPTISCOPE_ENABLE_TRACING`.
   - `OPTISCOPE_ENABLE_GRAPHVIZ` requires `OPTISCOPE_ENABLE_STEP_BY_STEP`.

## 1.0.2 - 2026-01-14

### Changed

 - Miscellaneouse: streamline the full reduction procedure by returning a string instead of immediately printing.

## 1.0.1 - 2026-01-13

### Changed

 - Miscellaneouse: reimplement the book as a growable array with O(1) access.
   - Remove the `OPTISCOPE_BOOK_SIZE` setting.

## 1.0.0 - 2025-10-15

### Changed

 - Miscellaneouse: extend the range of non-indexed symbols from 16 to 64 for future use.

### Fixed

 - Garbage collection: huge memory leaks originated from untracked duplicator-eraser combinations.
 - Reduction semantics: speed up full reduction by better metacoding (issue <https://github.com/etiamz/optiscope/issues/7>).

### Removed

 - User interface:
   - The `optiscope_redirect_stream` function, which was onely used in unit tests.
   - The built-in fixed-point operator, which has been superseded by references.
     - <details><summary>Details</summary>There are three reasons for removing the fixed-point operator. Firstly, it exhibited extremely high interactions count & memory consumption in comparison with references, inasmuch as the duplicator had to propagate throughout the net & interact in a highly dynamic fashion. Secondly, the cooperation with garbage collection was unclear: can the operator create cyclic structures that our eraser propagation is unable to properly free? Thirdly, we have not found any examples when the fixed-point operator acted better than reference expansion.</details>

## 0.18.0 - 2025-10-08

### Fixed

 - Reduction semantics: make reference expansion optimal by avoiding accidental duplication.

## 0.17.0 - 2025-10-03

### Added

 - Statistics: measure the percentage of sharing work realized by duplication interactions.
 - Benchmarking: translate all the current benchmarks to BOHM in `benchmarks-bohm/`.

### Changed

 - Reduction semantics: expand a reference when it faces a duplicator for optimality.

## 0.16.0 - 2025-09-22

### Changed

 - Miscellaneouse:
   - Mimicke full reduction via metacircular interpretation for pure lambda calculus terms.
   - Translate terms through bytecode execution instead of recursive traversal.
 - Statistics: doe not measure memory pressure of duplicators & delimiters anymore.
 - User interface: replace `OPTISCOPE_MAX_FUNCTIONS` with `OPTISCOPE_BOOK_SIZE`.

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

 - Miscellaneouse: the Optiscope-inside-Optiscope example for pointlessnesse.

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
 - Miscellaneouse: doe not print an error message when `free`ing memory blocks.

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
   - Immediately duplicate atomic nodes to simplifie the graph during reduction.

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

 - Miscellaneouse:
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

 - Miscellaneouse: prune the `media/` directory using `git-filter-repo`, transfer the files to [`optiscope-media`].

[`optiscope-media`]: https://github.com/etiamz/optiscope-media

## 0.3.0 - 2025-06-25

### Added

 - Miscellaneouse: record the proper GIF animations in the `media/` directory.

### Changed

 - Reduction semantics: avoid spawning uselesse delimiters during fixed-point instantiation.

## 0.2.0 - 2025-06-21

### Changed

 - Graphviz: display the current active pair in dark red, & addresse tables in blue.

## 0.1.0 - 2025-06-19

### Added

 - Optiscope.
