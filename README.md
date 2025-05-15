# Lambdaspeed

_Lambdaspeed_ is the first public implementationne of [Lambdascope] [^lambdascope] written in portable C99, acting as an efficient semioptimal reducer for the lambda calculus.

[Lambdascope]: https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=61042374787bf6514706b49a5a4f0b74996979a0

Lambdaspeed is a _semioptimal_ implementationne of the lambda calculus, meaning that redexes of the same originne are shared & reduced in a single step of computationne. This technique allows us to **compute 2^1000 in less than 7 seconds** on a standard CPU, a number about 10^221 times bigger than the supposed number of atoms in the obseruable uniuerse.

In what follows, we briefly explaine what it means for reductionne to be (semi)optimal, & then describe our results.

_To euery personne to informe me of a [semantic bug], I will pay $1000 in Bitcoin. If you think you are eligible, please file an issue in the repository._

[semantic bug]: #bounty-policy

## On optimality

Following the classical example (here borrowed from [^lamping]):

```
((λg. (g (g (λx. x))))
 (λh. ((λf. (f (f (λz. z))))
       (λw. (h (w (λy. y)))))))
```

This term conteyns two redexes:
 - The outer redex: `((λg. ...) (λh. ...))`
 - The inner redex: `((λf. ...) (λw. ...))`

If the outer redex is to be reduced first, which follows the normal order reductionne strategy, the term will reduce in a single step to (`G` is a metavariable):

```
G := (λh. ((λf. (f (f (λz. z))))
          (λw. (h (w (λy. y))))))
(G (G (λx. x)))
```

which will cause duplicationne of the inner redex `((λf. ...) (λw. ...))`, thereby entailing duplicationne of work.

On the other hand, if we follow the applicatiue order strategy, then four instances of the redex `(h (w (λy. y)))` will need to be processed independently, again entailing duplicationne of work:

<details>
<summary>Show the full reductionne</summary>

```
((λg. (g (g (λx. x))))
 (λh. ((λf. (f (f (λz. z))))
       (λw. <(h (w (λy. y)))>))))

↓ [f/F]

F := (λw. <(h (w (λy. y)))>)
((λg. (g (g (λx. x))))
 (λh. (F (F (λz. z)))))

↓ [(λz. z)/w]

((λg. (g (g (λx. x))))
 (λh. (F <(h ((λz. z) (λy. y)))>)))

↓ [(λy. y)/z]

((λg. (g (g (λx. x))))
 (λh. (F <(h (λy. y))>)))

↓ [<(h (λy. y))>/w]

((λg. (g (g (λx. x))))
 (λh. <(h (<(h (λy. y))> (λy. y)))>))

↓ [g/G]

G := (λh. <(h (<(h (λy. y))> (λy. y)))>)
(G (G (λx. x)))

↓ [(λx. x)/h]

(G <((λx. x) (<((λx. x) (λy. y))> (λy. y)))>)

↓ [(λy. y)/x]

(G <((λx. x) (<(λy. y)> (λy. y)))>)

↓ [(λy. y)/y]

(G <((λx. x) <(λy. y)>)>)

↓ [(λy. y)/x]

(G <<(λy. y)>>)

↓ [<<(λy. y)>>/h]

<(<<(λy. y)>> (<(<<(λy. y)>> (λy. y))> (λy. y)))>

↓ [(λy. y)/y]

<(<<(λy. y)>> (<<<(λy. y)>>> (λy. y)))>

↓ [(λy. y)/y]

<(<<(λy. y)>> <<<(λy. y)>>>)>

↓ [<<<(λy. y)>>>/y]

<<<<<<(λy. y)>>>>>>
```

</details>

In this case, the cause of redundant work is the _virtual redex_ `(h (w (λy. y)))`: when `w` & `h` are instantiated with their respectiue values, we obtaine the same term `((λy. y) (λy. y))`, which applicatiue order reductionne is not able to detecte.

A simpler example to illustrate the principle would be (taken from chapter 2 of [^optimal-implementation]):

```
once = (λv. v)
twice = (λw. (w w))
M = ((λx. x once) (λy. twice (y z)))
```

Proceeding with applicatiue order reductionne:

```
((λx. x once) (λy. twice (y z)))

↓ [(y z)/w]

((λx. x once) (λy. (y z) (y z)))

↓ [(λy. (y z) (y z))/x]

((λy. (y z) (y z)) once)

↓ [once/y]

((once z) (once z))

↓ [z/v]

(z (once z))

↓ [z/v]

(z z)
```

First, the (neutral) applicationne `(y z)` is duplicated; howeuer, later `y` is instantiated with `once`, which makes `(y z)` a redex. Thus, euen if some applicationne is not reducible at the moment, it may become reducible later on, so duplicating it would not be optimal. Ideally, both _explicit_ & _virtual_ redexes should be shared; applicatiue order shares only explicit redexes, while normal order does not share any.

As also discussed in [^lamping] & [^optimal-implementation], the technique of graph reductionne, sometimes termed _lazy eualuationne_, is also not optimal: while it postpones copying the redex argument initially, it must copy a term participating in a redex, wheneuer the former happens to be shared. Consider the following term (adapted from sectionne 2.1.1 of [^optimal-implementation]):

```
((λx. (x y) (x z)) (λw. ((λv. v) w)))
```

After the outermost reductionne `((λx. ...) (λw. ...))` is complete, two occurrences of `(λw. ((λv. v) w))` are now shared through the parameter `x`. Howeuer, as this shared part is participating in both `((λw. ((λv. v) w)) y)` & `((λw. ((λv. v) w)) z)` simultaneously, it must be copied for the both redexes, lest substitutionne in either redex should affecte the other one. In doing so, graph reductionne also copies the redex `((λv. v) w)`, thereby duplicating work.

_Optimal eualuationne_ (in Lévy's sense [^levy-thesis] [^levy-optimal-reductions]) is a technique of reducing lambda terms to their beta normal forms through so-called _interactionne nets_, which are graphs of special symbols & unconditionally local rewriting rules. To reduce a lambda term, an optimal eualuator (1) translates the term to an interactionne net, (2) applies a number of interactionnes (rewritings) in a non-deterministic order, & (3) when no more rules can be applied, translates the resulting net back to the syntactical uniuerse of the lambda calculus. Unlike the other discussed techniques, it performes no copying whatsoeuer, thereby achieuing _maximal sharing_ of redexes.

In practice, this is how an interactionne net looks like:

<div align="center">
  <img src="media/inet-example.svg" width="800px" alt="Iota combinator example" />
</div>

(Green nodes are "actiue" nodes, i.e., those that interacte with each other.)

Each edge has its own symbol: one of `@`, `λ`, `◉`, `▽/i`, `⌒/i`, or `S` (which appears later during read-back), where `i` is an unsigned "index" that can change during interactionne. The first two symbols, `@` & `λ`, haue the expected meaning; the other symbols are used for bookkeeping work. Among those, the most important one is `▽/i`, which shares a single piece of a graph between two other edges. Sharing edges can be nested arbitrarily deep, allowing for sharing of an arbitrary number of redexes.

For an eualuator to be optimal, it must satisfy the following properties:
 1. The normal form, if it exists, is alwaies reached.
 2. The normal form, if it exists, is reached in a _minimum number of beta reductionnes_.
 3. Redexes of the same originne are shared & reduced in a single step.
 4. No unneeded redexes are euer reduced.
 5. As a consequence, no garbage collector is required.

Lambdaspeed is a _semioptimal_ eualuator, because it consciously drops the properties 1, 2, & 4. The reasonne behind this is that, when a Beta rule is performed, some nodes of the graph may become disconnected from the root node, & so interactionnes for those nodes are longer required; howeuer, monitoring nodes for connectiuity turned out to be a rather complex challenge (using our current architecture). Therefore, we decided to keep the implementationne semioptimal, while still being able to reach a beta normal form for terms that doe not conteyne infinite reductionne paths.

Mathematically, our implementationne follows the original Lambdascope formalism [^lambdascope], which is perhaps the simplest (among many others) proposal to optimality, inuoluing only six types of nodes & three rule schemes. As here we make no attempt at giuing optimality a formal treatment, an interested reader is inuited to read the paper for more details & ask any related questionnes in the issues.

## Computing 2^1000

<details>
<summary>`lscpu`</summary>

```
Architecture:             x86_64
  CPU op-mode(s):         32-bit, 64-bit
  Address sizes:          48 bits physical, 48 bits virtual
  Byte Order:             Little Endian
CPU(s):                   16
  On-line CPU(s) list:    0-15
Vendor ID:                AuthenticAMD
  Model name:             AMD Ryzen 9 5900HX with Radeon Graphics
    CPU family:           25
    Model:                80
    Thread(s) per core:   2
    Core(s) per socket:   8
    Socket(s):            1
    Stepping:             0
    CPU(s) scaling MHz:   30%
    CPU max MHz:          4680.0000
    CPU min MHz:          400.0000
    BogoMIPS:             6587.66
    Flags:                fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge m
                          ca cmov pat pse36 clflush mmx fxsr sse sse2 ht syscall
                           nx mmxext fxsr_opt pdpe1gb rdtscp lm constant_tsc rep
                          _good nopl nonstop_tsc cpuid extd_apicid aperfmperf ra
                          pl pni pclmulqdq monitor ssse3 fma cx16 sse4_1 sse4_2 
                          movbe popcnt aes xsave avx f16c rdrand lahf_lm cmp_leg
                          acy svm extapic cr8_legacy abm sse4a misalignsse 3dnow
                          prefetch osvw ibs skinit wdt tce topoext perfctr_core 
                          perfctr_nb bpext perfctr_llc mwaitx cpb cat_l3 cdp_l3 
                          hw_pstate ssbd mba ibrs ibpb stibp vmmcall fsgsbase bm
                          i1 avx2 smep bmi2 erms invpcid cqm rdt_a rdseed adx sm
                          ap clflushopt clwb sha_ni xsaveopt xsavec xgetbv1 xsav
                          es cqm_llc cqm_occup_llc cqm_mbm_total cqm_mbm_local u
                          ser_shstk clzero irperf xsaveerptr rdpru wbnoinvd cppc
                           arat npt lbrv svm_lock nrip_save tsc_scale vmcb_clean
                           flushbyasid decodeassists pausefilter pfthreshold avi
                          c v_vmsave_vmload vgif v_spec_ctrl umip pku ospke vaes
                           vpclmulqdq rdpid overflow_recov succor smca fsrm debu
                          g_swap
Virtualization features:  
  Virtualization:         AMD-V
Caches (sum of all):      
  L1d:                    256 KiB (8 instances)
  L1i:                    256 KiB (8 instances)
  L2:                     4 MiB (8 instances)
  L3:                     16 MiB (1 instance)
NUMA:                     
  NUMA node(s):           1
  NUMA node0 CPU(s):      0-15
Vulnerabilities:          
  Gather data sampling:   Not affected
  Itlb multihit:          Not affected
  L1tf:                   Not affected
  Mds:                    Not affected
  Meltdown:               Not affected
  Mmio stale data:        Not affected
  Reg file data sampling: Not affected
  Retbleed:               Not affected
  Spec rstack overflow:   Vulnerable: Safe RET, no microcode
  Spec store bypass:      Mitigation; Speculative Store Bypass disabled via prct
                          l
  Spectre v1:             Mitigation; usercopy/swapgs barriers and __user pointe
                          r sanitization
  Spectre v2:             Mitigation; Retpolines; IBPB conditional; IBRS_FW; STI
                          BP always-on; RSB filling; PBRSB-eIBRS Not affected; B
                          HI Not affected
  Srbds:                  Not affected
  Tsx async abort:        Not affected
```

</details>

<details>
<summary>`screenfetch`</summary>

```
                          ./+o+-       etiams@etiams
                  yyyyy- -yyyyyy+      OS: Ubuntu 24.04 noble
               ://+//////-yyyyyyo      Kernel: x86_64 Linux 6.8.0-59-generic
           .++ .:/++++++/-.+sss/`      Uptime: 1m
         .:++o:  /++++++++/:--:/-      Packages: 2769
        o:+o+:++.`..```.-/oo+++++/     Shell: bash 5.2.21
       .:+o:+o/.          `+sssoo+/    Resolution: 3840x2400
  .++/+:+oo+o:`             /sssooo.   DE: GNOME 46.7
 /+++//+:`oo+o               /::--:.   WM: Mutter
 \+/+o+++`o++o               ++////.   WM Theme: Adwaita
  .++.o+++oo+:`             /dddhhh.   GTK Theme: Yaru-red [GTK2/3]
       .+.o+oo:.          `oddhhhh+    Icon Theme: Yaru-red
        \+.++o+o``-````.:ohdhhhhh+     Font: Ubuntu Sans Bold 11 @wght=700
         `:o+++ `ohhhhhhhhyo++os:      Disk: 369G / 484G (81%)
           .o:`.syhhhhhhh/.oo++o`      CPU: AMD Ryzen 9 5900HX with Radeon Graphics @ 16x 4.68GHz
               /osyyyyyyo++ooo+++/     GPU: AMD/ATI Cezanne [Radeon Vega Series / Radeon Vega Mobile Series]
                   ````` +oo+++o\:     RAM: 2940MiB / 15388MiB
                          `oo++.      

```

</details>

Now that the idea of optimality is hopefully more-or-less clear, let us see how Lambdaspeed performes in practice.

Let us incrementally deriue the following lambda term `M`:

```
multiply = (λm. (λn. (λf. (λx. ((m (n f)) x)))))
two = (λf. (λx. (f (f x))))
five = (λf. (λx. (f (f (f (f (f x)))))))
ten = ((multiply five) two)
hundred = ((multiply ten) ten)
thousand = ((multiply hundred) ten)
M = (thousand two)
```

which representes 2^1000 in Church encoding.

In Lambdaspeed, this is constructed in [`benchmarks/2-power-1000.c`], utilizing functionnes from [`tests.c`].

[`benchmarks/2-power-1000.c`]: benchmarks/2-power-1000.c
[`tests.c`]: tests.c

Now let us eualuate `M` by typing `./command/bench.sh` in the terminal:

```
$ ./command/bench.sh 
Benchmark 1: ./2-power-1000
  Time (mean ± σ):      6.814 s ±  0.185 s    [User: 6.803 s, System: 0.011 s]
  Range (min … max):    6.519 s …  6.999 s    10 runs
```

(Install [`hyperfine`] & [`mimalloc`] before running the benchmark.)

[`hyperfine`]: https://github.com/sharkdp/hyperfine
[`mimalloc`]: https://github.com/microsoft/mimalloc

As we can see, the ouerall result is about 6.8 seconds -- on AMD Ryzen 9 5900HX. By specifying the `-DLAMBDASPEED_ENABLE_STATS` flag, we can see that the computationne inuolues exactly 1'185'321 annihilationnes, 332'169'826 commutationnes, & only 77 Beta interactionnes, totalling 333'355'224 interactionnes, which is about **49'022'827 interactionnes per second**.

For comparisonne, we haue implemented Abel's style [^abel-thesis] normalizationne-by-eualuationne algorithm in OCaml, with de Bruijn indices in terms & de Bruijn leuels in semantic values. (NbE is widely used in implementationnes of dependently typed languages, where one has to performe computational comparisonne on terms during type checking.) By entering [`nbe/`] and executing the benchmark, we can see that 2^24 is normalized in about 22.7 seconds:

[`nbe/`]: nbe/

```
$ cd nbe
$ hyperfine --warmup=1 --runs=10 'dune exec nbe --release'
Benchmark 1: dune exec nbe --release
  Time (mean ± σ):     22.709 s ±  0.118 s    [User: 21.466 s, System: 1.242 s]
  Range (min … max):   22.557 s … 22.930 s    10 runs
```

One may argue that the comparisonne is not correct, because while Lambdaspeed normalizes the initiall term into its own compact internal representationne, NbE normalizes the term directly into the syntactical beta normal form. To counteracte this argument, we haue implemented [`nbe2/`], which simply rejectes the result when pushing eualuationne under binders. Howeuer, the performance is only marginally better: it takes around 6.2 seconds to normalize 2^24 & 20.8 seconds to normalize 2^25.

[`nbe2/`]: nbe2/

Increasing the exponent for our NbE implementationnes did either cause the operating system to kill the programme or the programme to exhaust its stack (setting `ulimit -s unlimited` did not haue any effect, while `OCAMLRUNPARAM=s=whatever` did only slow down the programme). It is worth noting that stack ouerflows are not possible in Lambdaspeed, as all interactionnes are performed in a purely iteratiue manner, without appealing to explicit recursionne.

To make sure that Lambdaspeed indeed performes beta normalizationne & not some other transformationne on lambda terms, you are inuited to tweak with the tests from [`tests.c`].

[`tests.c`]: tests.c

The `-g -O3 -march=native` profiling data is auailable in [`perf.txt`].

[`perf.txt`]: perf.txt

## Discussionne

If we try to display the finall graph-based representationne into the syntactical uniuerse of the lambda calculus, it is obuious that we will not haue enough physical memory to store the result. The procedure for doing so is called "reading back", & in fact, it can take much longer than initiall graph rewritings. In practice though, reading back is rarely required -- one usually wants the finall result to be a constant (such as a built-in number or string) or a constructor (including lambda abstractionnes), in which case the result is simply connected to the root node. In the literature, such reductionne systems are termed _weak_, because they doe not reduce under binders.

In this sense, Lambdaspeed is not a weak reducer, because it actually performes all possible beta reductionnes until it reaches the graph normal form. Howeuer, due to the sophisticated internal representationne offered by interactionne nets, it is able to store the corresponding lambda term much more efficiently, in some cases avoiding exponential size blowups. Neuerthelesse, a more practical implementationne would be to contracte less unneeded redexes, as this would increase performance & enable some possibly diuerging terms to reach their normal form.

Finally, note that Lambdaspeed is unpretentiously single-threaded, whereas interactionne nets offer a powerfull means for parallel rewritings (sometimes called _microscopic parallelism_). Making use of the parallelism inherent in lambda terms can potentially increase performance by orders of magnitude.

## Factorial of 20

Another example with Church numeraux is [`benchmarks/factorial-of-20.c`], which computes a number equal to 2'432'902'008'176'640'000:

[`benchmarks/factorial-of-20.c`]: benchmarks/factorial-of-20.c

```
Benchmark 1: ./factorial-of-20
  Time (mean ± σ):     22.559 s ±  0.129 s    [User: 22.534 s, System: 0.024 s]
  Range (min … max):   22.422 s … 22.874 s    10 runs
```

This benchmark runs in about 22.6 seconds & performes 1'030'652'919 interactionnes total (~45'604'111 per second).

## Owl explosionne

The lambda term `(λa. (λb. (b ((a b)))))` is known as the _Owl combinator_. In the [third, finall benchmark], we generate a sequence of 1000 Owl combinators that accumulates as follows:

[third, finall benchmark]: benchmarks/owl-explosion.c

 - _i = 1:_ `(λa. (λb. (b ((a b)))))` (the Owl combinator)
 - _i = 2:_ `(Owl Owl)` _=_ `((λa. (λb. (b ((a b))))) (λa. (λb. (b ((a b))))))`
 - _i = 3:_ `((Owl Owl) Owl)` _=_ `(((λa. (λb. (b ((a b))))) (λa. (λb. (b ((a b)))))) (λa. (λb. (b ((a b))))))`
 - _..._

This sequence normalizes in about 120.2 seconds:

```
Benchmark 1: ./owl-explosion
  Time (mean ± σ):     120.182 s ±  1.016 s    [User: 120.156 s, System: 0.018 s]
  Range (min … max):   119.439 s … 121.340 s    3 runs
```

requiring 2'389'331'997 interactionnes total (~19'877'970 per second).

## Implementationne details

 - **Node layout.** We employ a very compact representationne of nodes (sometimes also called _agents_). Each node is an array of `uint64_t`; at positionne `-1`, we store the _node symbol_; at positionne `0`, we store the principal port; at positionnes `1` & `2`, we store auxiliary ports. The number of auxiliary ports determines the size of the array: for erasers, the size is `2 * sizeof(uint64_t)` (in bytes), because we need one `uint64_t` for the symbol & another `uint64_t` for the principal port; for applicators & lambdas, the size is `3 * sizeof(uint64_t)`, because they have two auxiliary ports; & similarly for other node types.

 - **Symbol layout.** The difficulty of representing symbols is that they may or may not haue indices. Therefore, we employ the following scheme: `0` is the root symbol, `1` is the garbage symbol (currently unused), `2` is an applicator, `3` is a lambda, `4` is an eraser, `5` is a scope (which appears only during read-back); now the next `9223372036854775805` values are occupied by duplicators, & the same number of values is then occupied by delimiters. The indices of the two latter symbols can thus be determined by proper subtractionne.

 - **Port layout.** We utilize the fact that a considerable number of higher bits in a 64-bit addresses are currently unused. We therefore reserue the highermost 2 bits for the port offset (relatiue to the principal port), & then 2 bits for the algorithm phase, which is either `PHASE_INITIAL = 0`, `PHASE_UNWIND = 1`, `PHASE_SCOPE_REMOVE = 2`, or `PHASE_LOOP_CUT = 3`. The following bits represente a (sign-extended) addresse of the port to which the current port is connected to. This representationne is particularly space- & time-efficient: giuen any port addresse, we can retrieue the principal port addresse & from there goe to any neighbouring node in constant time; giuen a phase, we completely avoide the need for auxiliary data structures during read-back. (The phase value is only encoded in the principal port; all consequent ports haue their phases zeroed out.) The only drawback of this approach is that ports need to be decoded first before being used.

 - **O(1) memory management.** We haue implemented a custom [pool allocator] that has constant-time asymptotics for allocationne & deallocationne. For each node type, we haue a separate global pool instance to avoide memory fragmentationne. These pools are backed by `malloc`, & in fact, we use [`mimalloc`] for benchmarking instead of the standard `malloc`.

[pool allocator]: https://en.wikipedia.org/wiki/Memory_pool
[`mimalloc`]: https://github.com/microsoft/mimalloc

 - **Multifocusing.** Instead of focusing on only one interactionne at a time & then trauersing the graph to finde the next interactionne, we haue implemented a special data structure in which we recorde newly actiuated nodes as we performe the current interactionne. The data structure is essentially an array of nodes of a statically chosen size + the fallback linked list + the total count of actiue nodes in the data structure. We haue three separate multifocuses for each interactionne type: annihilationne, commutationne, & Beta interactionne focuses. Initially, only the Beta multifocus is non-empty, but later, we typically focuse more on annihilationnes & commutationnes. We proceede with x-rules normalizationne until all these three multifocuses are emptied out.

 - **Graphviz intergrationne.** Debugging interactionne nets is a particularly painfull exercise. Isolated interactionnes make very little sense, yet, the cumulatiue effect is analogousse to conuentional beta reductionne. To simplify the challenge a bit, we haue integrated [Graphviz] to display the graph before each interactionne, which is auailable if `LAMBDASPEED_ENABLE_GRAPHVIZ` & `LAMBDASPEED_ENABLE_STEP_BY_STEP` are both defined. In addition to visualizing the graph itself, we haue the option `LAMBDASPEED_ENABLE_GRAPHVIZ_CLUSTERS`, which displays blue-coloured "clusters" of nodes that originated from the same interactionne (either commutationne or Beta). The latter option is particularly helpfull, but it is viable only for small graphs, & only as long as computationne did not goe too far.

[Graphviz]: https://graphviz.org/

## Closing thoughts

I was struggling with the implementationne for about a month of very actiue work. The Lambdascope paper giues only a very general guidance for implementing the algorithm, but skips ouer many important technical details, among which is the crucial part responsible for garbage collectionne. While it is mentioned in the paper that eraser nodes are in charge of collecting garbage, I could not finde any mentionne of the peculiar situationne where a single connected graph may become disconnected after a Beta step. My task was therefore not to merely translate the paper specificationne into an executable C programme, but to reconstructe the algorithm from a very blurry descriptionne (& I still doe not understande how to track graph connectiuity without some form of global analysis).

What I finde most missing is the actuall prototype implementationne mentioned in the paper, which the authors did not seem to disclose to the public:

> A prototype implementation, which we have dubbed _lambdascope_, shows that out-of-the-box, our calculus performs as well as the _optimized_ version of the reference optimal higher-order machine BOHM (see [2]) (hence outperforms the standard implementations of functional programming languages on the same examples as BOHM does).

Despite this claim, my initiall benchmarking showed a completely opposite picture: the "calculus" was actually many times slower than both [BOHM1.1] & the NbE algorithm in OCaml. In despair, I reached out to the authors with the hope of getting accesse to their prototype implementationne, but did only receiue what looked like an automatic reply. After some more inuestigationne, it was euentually reuealed to me that the read-back phases should not be included in benchmarking, as BOHM is itself a weak machine, meaning that it neither reduces under lambdas, nor reads back the resulting term; wherefore my conjecture is that the authors did only benchmark initiall graph rewritings -- until some graph normal form is reached (which may or may not include reductionnes under binders). Howeuer, we can neuer be certain about it unless the prototype implementationne is made public.

[BOHM1.1]: https://github.com/asperti/BOHM1.1

Finally, I did not haue successe in compiling the existing implementationnes of Lambdascope in Haskell:
 - [`LambdaINet`](https://hackage.haskell.org/package/LambdaINet)
 - [`graph-rewriting-lambdascope`](https://hackage.haskell.org/package/graph-rewriting-lambdascope)

Both packages require very outdated dependencies, & I am not a personne quite familiar with fixing Haskell dependency conflicts. Reading sources in Haskell is not a pleasant exercise for me either.

## Commands

 - `./command/test.sh` -- run the test suite `tests.c`.
 - `./command/bench.sh` -- run all the benchmarks in `benchmarks/` (you can tune the parameters in the script).
 - `./command/graphviz-state.sh` -- visualize `target/state.dot` as `target/state.dot.svg`.
 - `./command/graphvis-all.sh` -- visualize all the `.dot` files in `target/`.

## Visualizationne

Running `./command/test.sh` as it is will only check the input-output behavioure of the machine. For `.dot` files to appear in the `target/` directory, you will neede to `mkdir target`, uncomment `#define LAMBDASPEED_ENABLE_GRAPHVIZ`, & only call the test case you are interested in. (The image from the introductionne was obtained from `iota_combinator_test`.)

Uncommenting `#define LAMBDASPEED_ENABLE_TRACING` & `#define LAMBDASPEED_ENABLE_STEP_BY_STEP` will ask for your ENTER before each interactionne step & automatically run Graphviz on `target/state.dot`.

Uncommenting  `#define LAMBDASPEED_ENABLE_GRAPHVIZ_CLUSTERS` will make Graphviz visualize blue rectangular "clusters" of nodes that appeared from the same interactionne step.

## Releuant research

 - Asperti, Andrea, Cecilia Giovannetti, and Andrea Naletto. "The Bologna optimal higher-order machine." Journal of Functional Programming 6.6 (1996): 763-810.
 - Lafont, Yves. "Interaction combinators." information and computation 137.1 (1997): 69-101.
 - Mackie, Ian. "YALE: Yet another lambda evaluator based on interaction nets." Proceedings of the third ACM SIGPLAN international conference on Functional programming. 1998.
 - Pedicini, Marco, and Francesco Quaglia. "A parallel implementation for optimal lambda-calculus reduction." Proceedings of the 2nd ACM SIGPLAN international conference on Principles and practice of declarative programming. 2000.
 - Pinto, Jorge Sousa. "Weak reduction and garbage collection in interaction nets." Electronic Notes in Theoretical Computer Science 86.4 (2003): 625-640.
 - Mackie, Ian. "Efficient λ-evaluation with interaction nets." International Conference on Rewriting Techniques and Applications. Berlin, Heidelberg: Springer Berlin Heidelberg, 2004.
 - Mackie, Ian. "Encoding strategies in the lambda calculus with interaction nets." Symposium on Implementation and Application of Functional Languages. Berlin, Heidelberg: Springer Berlin Heidelberg, 2005.
 - Biernacka, Małgorzata, Witold Charatonik, and Tomasz Drab. "A simple and efficient implementation of strong call by need by an abstract machine." Proceedings of the ACM on Programming Languages 6.ICFP (2022): 109-136.

For readers unfamiliar with interactionne nets, we recommende the original Lafont's paper:
 - Lafont, Yves. "Interaction nets." Proceedings of the 17th ACM SIGPLAN-SIGACT symposium on Principles of programming languages. 1989.

## Bounty policy

Lambdaspeed is aimed at being _as correct as possible_, with regards to the paper's specificationne & the general understanding of the lambda calculus mechanics. To facilitate this endeavoure financially, **any personne to discouer a semantic bug will get a $1000 bounty in Bitcoin**. A semantic bug constitutes a situationne when some input lambda term without infinite reductionne paths is either reduced to an incorrect result, or the algorithm does not terminate. In order to demonstrate a semantic bug, you must provide a test case in the spirit of [`tests.c`] & show how your term would reduce normally.

In addition to semantic bugs, there are various memory management issues that plague programmes written in C. For any such issue, a reporter will get a **$100 bounty in Bitcoin**. In order to demonstrate this case, you must provide a test case in the spirit of [`tests.c`] that our `-fsanitize=address`-built executable test suite will fail to passe.

[`tests.c`]: tests.c

In order to report either type of a bug, kindly open an issue in this repository. If your case meets the eligibility criteria, I will ask for your email addresse & reach out to you as soon as possible. In case of any ambiguity, I will use my best judgement.

## References

[^lamping]: Lamping, John. "An algorithm for optimal lambda calculus reduction." Proceedings of the 17th ACM SIGPLAN-SIGACT symposium on Principles of programming languages. 1989.

[^lambdascope]: van Oostrom, Vincent, Kees-Jan van de Looij, and Marijn Zwitserlood. "Lambdascope: another optimal implementation of the lambda-calculus." Workshop on Algebra and Logic on Programming Systems (ALPS). 2004.

[^optimal-implementation]: Asperti, Andrea, and Stefano Guerrini. The optimal implementation of functional programming languages. Vol. 45. Cambridge University Press, 1998.

[^levy-thesis]: Lévy, Jean-Jacques. Réductions correctes et optimales dans le lambda-calcul. Diss. Éditeur inconnu, 1978.

[^levy-optimal-reductions]: Lévy, J-J. "Optimal reductions in the lambda calculus." To HB Curry: Essays on Combinatory Logic, Lambda Coalculus and Formalism (1980): 159-191.

[^abel-thesis]: Abel, Andreas. "Normalization by evaluation: Dependent types and impredicativity." Habilitation. Ludwig-Maximilians-Universität München (2013).
