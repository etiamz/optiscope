# 🔬 Optiscope

_Optiscope_ is an experimental Lévy-optimal implementation of the pure lambda calculus enriched with native function calls, if-then-else expressions, & a fixed-point operator.

Being the first public implementation of [Lambdascope] [^lambdascope] written in portable C99, it is also the first interaction net reducer capable of calling user-provided functions at native speed. This combination allows one to (1) embed all primitive types & operations through C FFI, (2) interleave lazy evaluation with direct side effects, and (3) let optimal reduction share calls to pure C functions just like ordinary β-reductions. As such, Optiscope can be understood as an optimal λ-calculus reducer extended to sharing foreign computation.

[Lambdascope]: https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=61042374787bf6514706b49a5a4f0b74996979a0

In what follows, we briefly explaine what it means for reduction to be Lévy-optimal, & then describe our results.

_To every person to informe me of a semantic bug, I will pay $1000 in Bitcoin. More details [here](#bounty-policy)._

## Installation

```
$ git clone https://github.com/etiamz/optiscope.git
$ cd optiscope
$ ./command/test.sh
```

## Evaluation by Interaction

Optiscope offers a lightweight C API for writing programs in the extended lambda calculus; these programs are automatically translated to interaction nets under the hood. To see Optiscope in action, consider the example program [`examples/lamping-example.c`], which evaluates to the identity lambda under weak reduction to interface normal form [^interface-normal-form]:

[`examples/lamping-example.c`]: examples/lamping-example.c

```c
#include "../optiscope.h"

static struct lambda_term *
lamping_example(void) {
    struct lambda_term *g, *x, *h, *f, *z, *w, *y;

    return apply(
        lambda(g, apply(var(g), apply(var(g), lambda(x, var(x))))),
        lambda(
            h,
            apply(
                lambda(f, apply(var(f), apply(var(f), lambda(z, var(z))))),
                lambda(w, apply(var(h), apply(var(w), lambda(y, var(y))))))));
}

int
main(void) {
    optiscope_open_pools();
    optiscope_algorithm(NULL, lamping_example());
    puts("");
    optiscope_close_pools();
}
```

This is the exact Optiscope encoding of the example discussed by Lamping in [^lamping].

By adding the following lines into `optiscope.h`:

```c
#define OPTISCOPE_ENABLE_TRACING
#define OPTISCOPE_ENABLE_STEP_BY_STEP
#define OPTISCOPE_ENABLE_GRAPHVIZ
```

& typing `./command.example.sh lamping-example` in your shell, Optiscope will visualize each interaction step in `target/state.dot.svg` before you presse ENTER to fire the red interaction:

<div align="center">
  <a href="https://raw.githubusercontent.com/etiamz/optiscope-media/refs/heads/master/lamping-example-animation.gif">
    <img src="https://raw.githubusercontent.com/etiamz/optiscope-media/refs/heads/master/lamping-example-preview.png" width="750px" alt="Lamping's example" />
  </a>
</div>

## Side-Effectfull Evaluation

See the example [`examples/palindrome.c`](examples/palindrome.c) with a detailed step-by-step explanation in comments.

## Rules for Side Effects

In this section, we give well-formednesse rules for side-effectfull computations in Optiscope.

We take our inspiration from the use of monads in call-by-need functional programming languages, such as Haskell. The intuition is as follows: whenever we would write `_ <- action; k` in Haskell, we write `perform(action, k)` in Optiscope; whenever we would write `x <- comp; k` in Haskell, we write `bind(x, action, k)` in Optiscope; whenever we would end execution with `action` in Haskell (e.g., `putStrLn "goodbye"` at the end of do-notation), we doe the same in Optiscope.

We write `comp/pure` for pure computations, i.e., those not conteyning `bind` or `perform`, & `comp/impure` for computations involving side effects. The formal rules for impure computations are as follows:
 - If `f` is a side-effectfull function, then `unary_call(f, token)/impure`, where `token` is a cell passed downwards.
 - If `f` is a side-effectfull function & `rand/impure`, then `binary_call(f, rand, token)/impure`, where `token` is a cell passed downwards.
 - If `action/impure` & `k/impure`, then `perform(action, k)/impure`.
 - If `action/impure` & `k/impure` , then `bind(x, action, k)/impure`.
 - If `action/impure` & `if_then/impure` & `if_else/impure`, then `if_then_else(action, if_then, if_else)/impure`.
 - If `comp/pure`, then `comp/impure`.

The onely difference between Optiscope and Haskell-style monads is that, while monads are first-class citizens in Haskell, they merely manifest themselves as well-formednesse rules in Optiscope. While the Haskell approach allows for more flexibility & preserves the conceptual purity of I/O functions, it requires the I/O monad to be incarnated into the language & to be treated specially by the runtime system; on the other hand, the Optiscope approach onely requires carefull positioning of effectfull computation, which can be easily achieved through a superimposed type system that implements the rules above.

## On Performance

_Optimal XOR efficient?_ I made a [fairly non-trivial effort] at optimizing the implementation, including leveraging compiler- & platform-specific functionality, yet, [our benchmarks] revealed that optimal reduction à la Lambdascope performes many times worse than [unoptimized Haskell] & [unoptimized OCaml]; for instance, whereas Optiscope needs about 6 seconds to execute an insertion sort on a Scott-encoded list of onely 2000 elements (in decreasing order), the Haskell implementation handles 10'000 elements in just two seconds. In general, the nature of performance penalty caused by bookkeeping work is unclear; however, with increasing list sizes, the factor by which Optiscope runs slower than more traditional lambda calculus implementations is onely increasing.

[fairly non-trivial effort]: #implementation-details
[our benchmarks]: benchmarks/
[unoptimized Haskell]: benchmarks-haskell/
[unoptimized OCaml]: benchmarks-ocaml/

Similar stories can be told about the other benchmarks. At one moment, I wondered if I merely implemented Lambdascope incorrectly because of this excerpt from the paper:

```
A prototype implementation, which we have dubbed _lambdascope_, shows that
out-of-the-box, our calculus performs as well as the _optimized_ version of the
reference optimal higher-order machine BOHM (see [2]) (hence outperforms the
standard implementations of functional programming languages on the same
examples as BOHM does).
```

This is a very interesting excerpt, because while it is concerned with efficiency, it says nothing in particular about it, save that their implementation is as fast as BOHM. So the question is: how fast is BOHM?

I downloaded [BOHM1.1] & wrote the following Scott insertion sort benchmark for 1000 elements:

[BOHM1.1]: https://github.com/asperti/BOHM1.1

<details>
<summary>Show</summary>

```
def scottNil = \f.\g.g;;

def scottCons = \a.\b.\f.\g.(f a b);;

def scottSingleton = \x.(scottCons x scottNil);;

def lessEqual = \x.\y.if x <= y then 1 else 0;;

def scottInsert = rec scottInsert = \elem.\list.
  let onNil = (scottSingleton elem) in
  let onCons = \h.\t.
    if (lessEqual elem h) == 1 
      then (scottCons elem (scottCons h t)) 
      else (scottCons h (scottInsert elem t)) in
  (list onCons onNil);;

def scottInsertionSort = rec scottInsertionSort = \list.
  let onNil = scottNil in
  let onCons = \h.\t.(scottInsert h (scottInsertionSort t)) in
  (list onCons onNil);;

def scottSumList = rec scottSumList = \list.
  let onNil = 0 in
  let onCons = \h.\t.h + (scottSumList t) in
  (list onCons onNil);;

def generateList = rec generateList = \n.
  if n == 0 
    then scottNil 
    else (scottCons (n-1) (generateList (n-1)));;

def benchmarkTerm = 
  (scottSumList (scottInsertionSort (generateList 1000)));;

benchmarkTerm;;
```

</details>

Then I typed `#load "scott-insertion-sort.bohm";;` and... it hanged my computer.

For a couple of hundred elements, it executed almost instantly, but it was still of no rival to Haskell & OCaml.

Also read the following excerpt from [^optimal-implementation], section 12.4:

```
BOHM works perfectly well for pure λ-calculus: much better, in average, than all
"traditional" implementations. For instance, in many typical situations we have
a polynomial cost of reduction against an exponential one.
```

Interesting. What are these "typical situations"? In section 9.5, the authors provide detailed results for a few benchmarks: Church-numeral factorial, Church-numeral Fibonacci sequence, & finally two Church-numeral terms `λn.(n 2 I I)` & `λn.(n 2 2 I I)`. On the two latter ones, Caml Light & Haskell exploded on larger values of `n`, while BOHM was able to handle them.

Next:

```
Unfortunately, this is not true for "real world" functional programs. In this
case, BOHM's performance is about one order of magnitude worse than typical
call-by-value implementations (such as SML or Caml-light) and even slightly (but
not dramatically) worse than lazy implementations such as Haskell.
```

Looking at my benchmarks, I cannot call it "slightly worse", but rather "dramatically worse". The authors doe not elaborate what real-world programs they tested BOHM on, except for a quicksort algorithm from section 12.3.2 & the `append` function from section 12.4.1, for which they doe not provide comparison benchmarks with traditional implementations.

Finally, the authors conclude:

```
The main reason of this fact should be clear: we hardly use truly higher-order
_functionals_ in functional programming. Higher-order types are just used to
ensure a certain degree of parametricity and polymorphism in the programs, but
we never really consider functions as _data_, that is, as the real object of the
computation -- this makes a crucial difference with the pure λ-calculus, where
all data are eventually represented as functions.
```

Well, in our benchmarks, we represent all data besides primitive integers as functions. Nonethelesse, BOHM was still much slower than Haskell & OCaml with optimizations turned off.

What conclusions should we draw from this? Have Haskell & OCaml so advanced in efficiency over the decades? Or does BOHM demonstrate superior performance on Churh numerals onely? Should we invest our time in making optimality efficient, given that simpler, unoptimized approaches prove to be more performant? I have no definite answer to these questions, but the point is: Optiscope is onely meant to incorporate native function calls into optimal reduction, & it is by no means to be understood as a reduction machine aimed at maximum efficiency. While it is true that Optiscope is a heavily optimized implementation of Lambdascope, this fact does not entail that it is immediately faster than more traditional approaches. For now, if you want to develop a high-performance call-by-need functional machine, it is presumably better to take the well-known Spinelesse Taglesse G-machine [^stg-machine] as a starting point.

Now, there are two possible avenues to mitigate the performance issue. The first one: since interaction nets provide a natural means for parallelization (sometimes termed _microscopic parallelisme_), it is possible to distribute interactions across many CPU/GPU cores, or even utilize a whole computing cluster. The second one: it should be possible to apply a supercompilation passe before optimal reduction, to make it statically remove administrative annihilation/commutation interactions that plague most of the computation. (For a five-minute introduction to supercompilation, see [Mazeppa's README].) Of course, it is possible to mix both of the approaches, in which case optimal reduction might indeed become a strong competitor to traditional reduction methods.

[Mazeppa's README]: https://github.com/mazeppa-dev/mazeppa/blob/master/README.md

## Implementation Details

 - **Node layout.** We interpret each graph node as an array `a` of `uint64_t` values. At position `a[-1]`, we store the _node symbol_; at `a[0]`, we store the principal port; at positions from `a[1]` to `a[3]` (inclusively), we store the auxiliary ports; at positions starting from `a[4]`, we store additional data elements, such as function pointers or computed cell values. The number of auxiliary ports & additional data elements determines the total size of the array: for erasers, the size in bytes is `2 * sizeof(uint64_t)`, as they need one position for the symbol & another one for the principal port; for applicators & lambdas having two auxiliary ports, the size is `3 * sizeof(uint64_t)`; for unary function calls, the size is `4 * sizeof(uint64_t)`, as they have one symbol, two auxiliary ports, & one function pointer. Similar calculation can be done for all the other node types.

 - **Symbol layout.** The difficulty of representing node symbols is that they may or may not have indices. Therefore, we employ the following scheme: `0` is the root symbol, `1` is an applicator, `2` is a lambda, `3` is an eraser, `4` is a cell, & so on until value `15`, inclusively; now the next `9223372036854775800` values are occupied by duplicators, & the same number of values is then occupied by delimiters. Together, all symbols occupy the full range of `uint64_t`. The indices of duplicator & delimiter symbols can be determined by proper subtraction, but in most cases, they can be compared without any preprocessing.

 - **Port layout.** Modern x86-64 CPUs utilize the 48-bit addresse space, leaving 16 highermost bits unused (i.e., sign-extended). We therefore utilize the highermost 2 bits for the port offset (relative to the principal port), & then 4 bits for the node phase, which can be `PHASE_REDUCTION`, `PHASE_GC`, `PHASE_GC_AUX`, or `PHASE_STACK`. The following bits constitute a (sign-extended) addresse of the port to which the current port is connected to. This layout is particularly space- & time-efficient: given any port addresse, we can retrieve the principal port & from there goe to any neighbouring node in constant time; with mutable phases, we avoid the need for additional data structures. (The phase value is onely encoded in the principal port; all consequent ports have their phases zeroed out.) The onely drawback of this approach is that ports need to be encoded when being assigned & decoded upon use.

 - **O(1) memory management.** We have implemented a custom [pool allocator] that has constant-time asymptotics for allocation & deallocation. For each node size, we have a separate global pool instance to avoid memory fragmentation. On Linux, these pools allocate 2MB huge pages that lessen frequent TLB misses, to account for cases when many nodes are to be manipulated; if either huge pages are not supported or Optiscope is running on a non-Linux system, we default to `malloc`.

[pool allocator]: https://en.wikipedia.org/wiki/Memory_pool

 - **Weak reduction.** In real situations, the result of pure lazy computation is expected to be either a constant value or a top-level constructor. Even when one seeks reduction under binders & other constructors, one usually also wants [controlling definition unfoldings] or reusing already performed unfoldings [^taming-supercompilation] to keep resulting terms manageable. We therefore adopt BOHM-style _weak reduction_ [^bohm] as the onely phase of our algorithm. Weak reduction repeatedly reduces the _leftmost outermost_ interaction until a constructor node (i.e., either a lambda abstraction or cell value) is connected to the root, reaching an interface normal form. This phase directly implements Lévy-optimal reduction by performing onely needed work, i.e., avoiding to work on an interaction whose result will be discarded later.<br>(A shocking side note: per section "5.6 Optimal derivations" of [^optimal-implementation], a truely optimal machine must necessarily be sequential, because otherwise, the machine risks at working on unneeded interactions!)

[controlling definition unfoldings]: https://andraskovacs.github.io/pdfs/wits24prez.pdf

 - **Garbage collection.** Specific types of interactions may cause whole subgraphs to be fully or partially disconnected from the root, such as when a lambda application rejects its operand or when an if-then-else node selects the correct branch, rejecting the other one. In order to battle memory leaks during weak reduction, we implement _eraser-passing garbage collection_ described as follows. When our algorithm determines that the most recent interaction has rejected one of its connections, our garbage collector commences incremental propagation of erasers by connecting a newly spawned eraser to the rejected port; iteratively, garbage collection at a specific port necessarily results in either freeing the node in question & continuing the propagation to its immediate neighbours _or_ leaving the eraser connection untouched, when the former operation cannot be carried out safely. (However, we doe also eliminate some uselesse duplicator-eraser combinations as discussed in the paper, which has a slightly different semantics.)<br>Our rules are inspired by Lamping's algorithm [^lamping] / BOHM [^bohm]: although perfectly local, constant-time graph operations, they doe not count as interaction rules, since garbage collection can easily happen at any port, including non-principal ones.

 - **Special lambdas.** We divide lambda abstractions into four distinct categories: (1) `SYMBOL_GC_LAMBDA`, lambdas with no parameter usage, called _garbage-collecting lambdas_; (2) `SYMBOL_LAMBDA`, lambdas with at least one parameter usage, called _relevant lambdas_; (3) `SYMBOL_LAMBDA_C`, relevant lambdas without free variables; & finally (4) `SYMBOL_IDENTITY_LAMBDA`, identity lambdas. Although onely one category is sufficient to expresse all of computation, we employ this distinction for optimization purposes: if we know the lambda category at run-time, we can implement the reduction more efficiently. For instance, instantiating an identity lambda boils down to simply connecting the argument to the root port, without spawning more delimiters; likewise, commutation of a delimiter node with `SYMBOL_LAMBDA_C` boils down to simply removing the delimiter, as suggested in section 8.1 of the paper.

 - **Delimiter compression.** When the machine detects a sequence of chained delimiters of the same index, it compresses the sequence into a single delimiter node endowed with the number of compressed nodes; afterwards, this new node behaves just as the whole sequence of delimiters would, thereby requiring significantly lesse interactions. The machine performes this operation both statically & dynamically: statically during the translation of the input lambda term, dynamically during delimiter commutations. In the latter case, i.e., when the current delimiter commutes with another node of arbitrary type, the machine performes the commutaion & checks whether the commuted delimiter(s) can be merged with adjacent delimiters, if any. With this optimization, the oracle becomes dozens of times faster on some benchmarks & uncomparably faster on others (e.g., `benchmarks/scott-quicksort.c`).
   - C.f. [_run-length encoding_](https://en.wikipedia.org/wiki/Run-length_encoding).

 - **Barriers.** It is now natural to prioritize delimiter compression, so that more delimiters get compressed. One way to accomplish this is to "freeze" certain interactions of delimiters with applicators: roughly, instead of repeatedly propagating uncompressed delimiters to the root, we can first compresse as many delimiters as we can, & onely then propagate this single compressed delimiter to the root. In order to realize this scheme, we employ so-called downwards-pointing _barriers_, which appear dynamically whenever a delimiter meets an applicator. In the graph, this situation is depicted as "🚧 _n_", where _n_ is the number of collected zero-indexed delimiters. Initially, _n_ is one, but when the barrier meets another zero-indexed delimiter, _n_ is incremented. Contrariwise, when the barrier meets some other node, it is transformed into an upwards-pointing delimiter that commutes with its applicator. According to our benchmarks, this kind of prioritization can reduce the total number of graph rewrites by almost a half.

 - **Delimiter extrusion.** If a delimiter points to the output port of some operator, it appears profitable to _extrude_ it to the operands, causing its residuals to interact with other nodes earlier in the reduction. Keeping the delimiter inactive until the operator returns a value results in much lower performance, because we want delimiters to disappear as early as possible.

 - **References.** Following HVM's terminology, a _reference_ is a special node that lazily expands to a lambda term, which is then translated to a corresponding net in a single interaction. Strictly speaking, references doe not contribute to the expressivenesse of the system, but they tend to be farre more efficient than our (optimal!) fixed-point operator. In addition to improved efficiency, references naturally support mutual recursion, because every Optiscope reference corresponds to a C function taking zero parameters & returning a lambda term. In the benchmarks, we prefer to implement recursion on the basis of references.

 - **Graphviz intergration.** Debugging interaction nets is a particularly painfull exercise. Isolated interactions make very little sense, yet, the cumulative effect is somehow analogous to conventional reduction. To simplifie the challenge a bit, we have integrated [Graphviz] (in debug mode onely) to display the whole graph between consecutive interaction steps, if requested at compile-time in `optiscope.h`. Alongside each node, our visualization also displays an ASCII table of port addresses, which has proven to be extremely helpfull in debugging various memory management issues in the past. (Previously, in addition to visualizing the graph itself, we used to have the option to display blue-coloured "clusters" of nodes that originated from the same interaction (either commutation or Beta); however, it was viable onely for small graphs, & onely as long as computation did not goe too farre.)

[Graphviz]: https://graphviz.org/

## Limitations

 - Despite that interaction nets allow for a _huge_ amount of parallelisme, Optiscope is an unpretentiously sequential reducer. We doe not plan to make it parallel, because it is unclear how to preserve Lévy-optimality in this case.
 - We doe not guarantee what will happen with ill-formed terms, such as when an if-then-else expression accepts a lambda as a condition. In general, we simply decide to commute such agents, but the overall result can be hard to predict.
 - Optiscope cannot detect when two syntactically idential configurations occur at run-time; that is, the avoidance of redex duplication is relative to the initiall term, not to the computational pattern exhibited by the term.
 - On conventional problems, Optiscope is in fact many times slower than traditional implementations, wherefore it is more of an interesting experiment rather than a production-ready technology.

## Acknowledgements

Thanks to Marvin Borner, Marc Thatcher, & Vincent van Oostrom for interesting discussions about optimality & side effectfull computation.

## Bounty Policy

Optiscope is aimed at being _as correct as possible_, with regards to the Lambdascope specification & the general understanding of the lambda calculus. To back up this endeavor financially, **any person to discover a semantic bug will get a $1000 bounty in Bitcoin**. A semantic bug constitutes a situation when some input lambda term is either reduced to an incorrect result, or the algorithm does not terminate. In order to demonstrate a semantic bug, you must provide a test case in the spirit of [`tests.c`] & show how your term would reduce normally. (Non-termination bugs are the trickiest, because it might be unclear if the machine is just inefficient or it is falling in a cycle. In order to prove your case, it might be necessary to pinpoint the _cause_ of non-termination, instead of merely the _fact_ of it.)

In addition to semantic bugs, there are various memory management issues that plague programs written in C. For any such issue, a reporter will get a **$100 bounty in Bitcoin**. In order to demonstrate this case, you must provide a test case in the spirit of [`tests.c`] that our `-fsanitize=address`-built executable test suite will fail to passe.

[`tests.c`]: tests.c

In order to report either type of a bug, kindly open an issue in this repository. If your case meets the eligibility criteria, I will aske for your email addresse & reach out to you as soon as possible. In case of any ambiguity, I will use my best judgement.

Semantic bugs related to extra functionality like native function calls & if-then-else nodes are not eligible for bounty, as they exist outside the realm of the pure lambda calculus.

## References

[^lamping]: Lamping, John. "An algorithm for optimal lambda calculus reduction." Proceedings of the 17th ACM SIGPLAN-SIGACT symposium on Principles of programming languages. 1989.

[^lambdascope]: van Oostrom, Vincent, Kees-Jan van de Looij, and Marijn Zwitserlood. "Lambdascope: another optimal implementation of the lambda-calculus." Workshop on Algebra and Logic on Programming Systems (ALPS). 2004.

[^optimal-implementation]: Asperti, Andrea, and Stefano Guerrini. The optimal implementation of functional programming languages. Vol. 45. Cambridge University Press, 1998.

[^interface-normal-form]: Pinto, Jorge Sousa. "Weak reduction and garbage collection in interaction nets." Electronic Notes in Theoretical Computer Science 86.4 (2003): 625-640.

[^stg-machine]: Jones, Simon L. Peyton. "Implementing lazy functional languages on stock hardware: the Spineless Tagless G-machine Version 2.5." (1993).

[^bohm]: Asperti, Andrea, Cecilia Giovannetti, and Andrea Naletto. "The Bologna optimal higher-order machine." Journal of Functional Programming 6.6 (1996): 763-810.

[^taming-supercompilation]: Jonsson, Peter A., and Johan Nordlander. "Taming code explosion in supercompilation." Proceedings of the 20th ACM SIGPLAN workshop on Partial evaluation and program manipulation. 2011.
