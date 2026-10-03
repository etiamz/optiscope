# Benchmarks

<details>
<summary>System information</summary>

Machine:

```
$ fastfetch --logo none --structure OS:Host:Kernel:CPU:GPU:Memory
OS: macOS Tahoe 26.6 (25G72) arm64
Host: MacBook Pro (16-inch, M5 Max, 2026)
Kernel: Darwin 25.6.0
CPU: Apple M5 Max (6+12) @ 4.61 GHz
GPU: Apple M5 Max (40) @ 1.62 GHz [Integrated]
Memory: 27.97 GiB / 128.00 GiB (22%)
```

Compiler:

```
$ clang --version | head -n1
Homebrew clang version 23.1.1
```

</details>

To observe the performance characteristics of optimal reduction à la Lambdascope, we present a
number of benchmarks that expose different computational patterns.

## Comparison With BOHM1.1

The following table compares Optiscope & [BOHM1.1] on matching inputs. The sorting benchmarks
operate on descending Scott-encoded lists of machine integers, summing up the elements after
sorting in order to reach WHNF:

[BOHM1.1]: https://github.com/asperti/BOHM1.1/tree/52d826aedbb00f0bd513d8bcbf2fc3fae1b758d2

| Input | Optiscope rewrites | BOHM rewrites | Optiscope peak nodes | BOHM peak nodes |
| --- | ---: | ---: | ---: | ---: |
| _ackermann(3, 5)_ | 1,803,983 | 2,402,589 | 3,194 | 1,801,908 |
| _ackermann(3, 6)_ | 7,320,653 | 9,713,641 | 6,394 | 7,288,429 |
| _ackermann(3, 7)_ | 29,494,987 | 39,064,691 | 12,794 | 29,321,217 |
| _ackermann(3, 8)_ | 118,408,009 | - | 25,594 | >100,000,000 |
| _takeuchi(24, 7, 3)_ | 4,540,994 | 4,484,118 | 1,348 | 3,347,769 |
| _takeuchi(24, 8, 3)_ | 21,865,709 | 22,055,999 | 1,399 | 16,675,714 |
| _takeuchi(24, 9, 3)_ | 90,615,854 | 93,328,122 | 1,450 | 71,410,066 |
| _takeuchi(24, 10, 3)_ | 332,010,611 | - | 1,501 | >100,000,000 |
| _bsort(25)_ | 145,093 | 286,858 | 3,003 | 99,633 |
| _bsort(50)_ | 946,993 | 2,096,858 | 7,803 | 736,283 |
| _bsort(150)_ | 21,973,343 | 53,318,108 | 67,361 | 18,970,383 |
| _bsort(300)_ | 168,844,118 | - | 269,561 | >100,000,000 |
| _isort(50)_ | 136,588 | 1,460,023 | 4,107 | 216,297 |
| _isort(100)_ | 530,563 | 10,994,973 | 8,057 | 1,692,147 |
| _isort(500)_ | 12,952,363 | - | 39,657 | >100,000,000 |
| _isort(1000)_ | 51,654,613 | - | 79,157 | >100,000,000 |
| _msort(50)_ | 95,409 | 2,044,302 | 1,905 | 1,150,888 |
| _msort(100)_ | 256,038 | 13,490,667 | 3,217 | 9,202,693 |
| _msort(500)_ | 3,313,882 | - | 13,338 | >100,000,000 |
| _msort(1000)_ | 11,329,334 | - | 26,712 | >100,000,000 |
| _qsort(50)_ | 283,814 | 3,216,795 | 10,982 | 470,550 |
| _qsort(100)_ | 1,109,989 | 24,096,195 | 34,257 | 3,368,275 |
| _qsort(500)_ | 27,249,389 | - | 700,028 | >100,000,000 |
| _qsort(1000)_ | 108,748,639 | - | 2,760,834 | >100,000,000 |
| _nqueens(5)_ | 137,152 | 577,712 | 703 | 483,258 |
| _nqueens(6)_ | 603,451 | 3,558,354 | 804 | 3,155,775 |
| _nqueens(7)_ | 2,692,572 | 22,250,481 | 941 | 20,746,267 |
| _nqueens(8)_ | 12,872,629 | - | 1,128 | >100,000,000 |
| _nqueens(9)_ | 64,329,574 | - | 1,532 | >100,000,000 |
| _nqueens(10)_ | 332,952,805 | - | 2,114 | >100,000,000 |
| _nqueens(11)_ | 1,854,676,010 | - | 4,448 | >100,000,000 |

Among the problem instances completed by both reducers, Optiscope reduces total graph rewrites by
factors of approximately 1.3 for Ackermann, 2.0-2.4 for bubble sort, 11-21 for insertion sort, 21-53
for merge sort, 11-22 for quicksort, & 4.2-8.3 for N-queens. On _takeuchi(24, 7, 3)_, Optiscope
requires approximately 1.3% more rewrites than BOHM, but 0.9% & 2.9% fewer on _takeuchi(24, 8, 3)_ &
_takeuchi(24, 9, 3)_, respectively. Optiscope's peak node counts are lower in every completed
comparison, by factors ranging from approximately 33 for _bsort(25)_ to 49,000 for _takeuchi(24, 9,
3)_. Optiscope successfully completes all 31 problem instances; BOHM exceeds the 100,000,000-node
limit on the remaining 13.

Notes:
 - Both totals count all local, constant-time graph rewrites performed during reduction, including
   garbage collection & optimizing rewrites. BOHM's counters were extended to include rewrites
   omitted from its original interaction count.
 - Peak node counts show the maximum number of nodes present in the graph during reduction.
 - Using rewrite totals & peak node counts allows us to compare computational work & space usage
   independently of compiler optimizations & machine speed.
 - BOHM implements recursion through a fixed-point operator, whereas Optiscope uses reference
   expansion. We conjecture that BOHM's fixed-point operator contributes to its higher peak node
   counts, but the comparison does not separate this contribution from differences in garbage
   collection & scope management.
 - `-` in the BOHM rewrites column indicates that BOHM exceeded the limit of 100,000,000
   simultaneously live nodes.
 - All the Optiscope runs completed successfully.

The BOHM benchmarks used for the comparison live in [`../benchmarks-bohm/`].

[`../benchmarks-bohm/`]: ../benchmarks-bohm/

## Optiscope Timings

On GNU/Linux, you need to reserve huge pages as follows: `sudo sysctl vm.nr_hugepages=6000`.

"Sharing work" counts all interactions involving duplicators, excluding interactions of duplicators
with delimiters, barriers, or segments. "Bookkeeping work" counts all rewrites involving delimiters,
barriers, or segments, except when these agents are erased at GC time. "Compression work" counts
delimiter-merging rewrites alone (which are also counted as bookkeeping work). The other statistical
counters are self-explanatory. Our rationale is to separate the work performed by Lamping's
simplified algorithm from the work performed by Optiscope's oracle implementation, so that we can
tracke the effects of our optimizations on the latter.

### [Ackermann function](ackermann.c)

Description: Computes the Ackermann function with initial values _(3, 8)_.

```
Benchmark 1: ./ackermann
  Time (mean ± σ):     870.9 ms ±   9.1 ms    [User: 863.5 ms, System: 5.7 ms]
  Range (min … max):   855.7 ms … 878.9 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 118408009
  Total interactions: 50152048
   Family reductions: 5571998
        Sharing work: 8.24%
    Bookkeeping work: 32.94%
             GC work: 41.17%
    Compression work: 0.00%
Max duplicator index: 0
 Max delimiter index: 0
     Peak node count: 25594
```

</details>

### [Takeuchi function](tak.c)

Description: Computes the Takeuchi function with initial values _(24, 9, 3)_.

```
Benchmark 1: ./tak
  Time (mean ± σ):     637.6 ms ±   1.3 ms    [User: 632.6 ms, System: 3.5 ms]
  Range (min … max):   636.1 ms … 639.1 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 90615854
  Total interactions: 38890919
   Family reductions: 4666911
        Sharing work: 11.16%
    Bookkeeping work: 29.61%
             GC work: 45.92%
    Compression work: 0.00%
Max duplicator index: 0
 Max delimiter index: 0
     Peak node count: 1450
```

</details>

### [Scott list bubble sort](scott-bubble-sort.c)

Description: Performes a bubble sort on a Scott-encoded list of 300 cells, then sums all the cells
up.

```
Benchmark 1: ./scott-bubble-sort
  Time (mean ± σ):      1.346 s ±  0.008 s    [User: 1.339 s, System: 0.006 s]
  Range (min … max):    1.336 s …  1.356 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 168844118
  Total interactions: 165301706
   Family reductions: 814219
        Sharing work: 96.16%
    Bookkeeping work: 1.93%
             GC work: 1.24%
    Compression work: 0.00%
Max duplicator index: 602
 Max delimiter index: 0
     Peak node count: 269561
```

</details>

### [Scott list insertion sort](scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 1000 cells, then sums all the
cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):     446.4 ms ±   4.3 ms    [User: 443.5 ms, System: 1.9 ms]
  Range (min … max):   442.9 ms … 453.5 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 51654613
  Total interactions: 18061044
   Family reductions: 5023014
        Sharing work: 3.87%
    Bookkeeping work: 37.91%
             GC work: 44.60%
    Compression work: 0.00%
Max duplicator index: 0
 Max delimiter index: 0
     Peak node count: 79157
```

</details>

### [Scott list merge sort](scott-merge-sort.c)

Description: Performes a merge sort on a Scott-encoded list of 1000 cells, then sums all the cells
up.

```
Benchmark 1: ./scott-merge-sort
  Time (mean ± σ):      94.9 ms ±   0.9 ms    [User: 93.4 ms, System: 0.9 ms]
  Range (min … max):    93.3 ms …  95.6 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 11329334
  Total interactions: 10031750
   Family reductions: 296262
        Sharing work: 81.12%
    Bookkeeping work: 9.13%
             GC work: 6.66%
    Compression work: 0.00%
Max duplicator index: 2002
 Max delimiter index: 0
     Peak node count: 26712
```

</details>

### [Scott list quicksort](scott-quicksort.c)

Description: Performes a quicksort on a Scott-encoded list of 1000 cells, then sums all the cells
up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):      1.054 s ±  0.006 s    [User: 1.044 s, System: 0.008 s]
  Range (min … max):    1.047 s …  1.062 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 108748639
  Total interactions: 55068043
   Family reductions: 15029014
        Sharing work: 11.03%
    Bookkeeping work: 49.75%
             GC work: 21.26%
    Compression work: 0.00%
Max duplicator index: 2000
 Max delimiter index: 0
     Peak node count: 2760834
```

</details>

### [N-queens](nqueens.c)

Description: Solves the 10-queens problem using Scott-encoded lists.

```
Benchmark 1: ./nqueens
  Time (mean ± σ):      2.554 s ±  0.016 s    [User: 2.542 s, System: 0.009 s]
  Range (min … max):    2.539 s …  2.581 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 332952805
  Total interactions: 169205278
   Family reductions: 21150593
        Sharing work: 27.95%
    Bookkeeping work: 25.81%
             GC work: 35.62%
    Compression work: 0.00%
Max duplicator index: 20
 Max delimiter index: 0
     Peak node count: 2114
```

</details>
