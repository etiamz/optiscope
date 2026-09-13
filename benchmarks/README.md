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
$ gcc-16 --version | head -n1
gcc-16 (Homebrew GCC 16.2.0) 16.2.0
```

</details>

To observe the performance characteristics of optimal reduction à la Lambdascope, we present a
number of benchmarks that expose different computational patterns.

## Comparison With BOHM1.1

The following table compares Optiscope & [BOHM1.1] on matching inputs. The sorting benchmarks
operate on descending Scott-encoded lists of machine integers, summing up the elements after
sorting in order to reach WHNF:

| Input | Optiscope rewrites | BOHM rewrites | Optiscope peak nodes | BOHM peak nodes |
| --- | ---: | ---: | ---: | ---: |
| _ackermann(3, 5)_ | 1,487,688 | 2,402,589 | 2,942 | 1,801,908 |
| _ackermann(3, 6)_ | 6,033,049 | 9,713,641 | 5,886 | 7,288,429 |
| _ackermann(3, 7)_ | 24,298,730 | 39,064,691 | 11,774 | 29,321,217 |
| _ackermann(3, 8)_ | 97,530,171 | - | 23,550 | >100,000,000 |
| _takeuchi(24, 7, 3)_ | 5,081,109 | 4,484,118 | 1,407 | 3,347,769 |
| _takeuchi(24, 8, 3)_ | 24,451,833 | 22,055,999 | 1,461 | 16,675,714 |
| _takeuchi(24, 9, 3)_ | 101,280,294 | 93,328,122 | 1,515 | 71,410,066 |
| _takeuchi(24, 10, 3)_ | 370,916,108 | - | 1,569 | >100,000,000 |
| _bsort(25)_ | 187,417 | 286,806 | 3,133 | 99,580 |
| _bsort(50)_ | 1,216,067 | 2,096,756 | 8,083 | 736,205 |
| _bsort(150)_ | 27,805,667 | 53,317,806 | 68,237 | 18,970,205 |
| _bsort(300)_ | 212,458,817 | - | 271,337 | >100,000,000 |
| _isort(50)_ | 143,576 | 1,460,019 | 3,785 | 216,223 |
| _isort(100)_ | 564,601 | 10,994,969 | 7,435 | 1,692,023 |
| _isort(500)_ | 13,922,801 | - | 36,635 | >100,000,000 |
| _isort(1000)_ | 55,595,551 | - | 73,135 | >100,000,000 |
| _msort(50)_ | 143,874 | 2,044,194 | 4,032 | 1,150,810 |
| _msort(100)_ | 435,255 | 13,490,459 | 10,892 | 9,202,565 |
| _msort(500)_ | 7,346,531 | - | 185,315 | >100,000,000 |
| _msort(1000)_ | 27,135,694 | - | 703,375 | >100,000,000 |
| _qsort(50)_ | 362,143 | 3,216,791 | 11,929 | 470,476 |
| _qsort(100)_ | 1,436,668 | 24,096,191 | 39,340 | 3,368,151 |
| _qsort(500)_ | 35,682,868 | - | 909,968 | >100,000,000 |
| _qsort(1000)_ | 142,615,618 | - | 3,605,397 | >100,000,000 |
| _nqueens(5)_ | 142,397 | 577,712 | 703 | 483,248 |
| _nqueens(6)_ | 642,117 | 3,558,354 | 790 | 3,155,771 |
| _nqueens(7)_ | 2,910,329 | 22,250,541 | 997 | 20,746,272 |
| _nqueens(8)_ | 14,151,747 | - | 1,238 | >100,000,000 |
| _nqueens(9)_ | 71,832,021 | - | 1,956 | >100,000,000 |
| _nqueens(10)_ | 377,505,855 | - | 3,004 | >100,000,000 |
| _nqueens(11)_ | 2,133,946,241 | - | 7,571 | >100,000,000 |

Among the problem instances completed by both reducers, Optiscope reduces total graph rewrites by
factors of approximately 1.6 for Ackermann, 1.5-1.9 for bubble sort, 10-19 for insertion sort, 14-31
for merge sort, 9-17 for quicksort, & 4-8 for N-queens. For Takeuchi, Optiscope requires
approximately 9-13% more rewrites than BOHM. Optiscope's peak node counts are lower in every
completed comparison, by factors ranging from approximately 32 for _bsort(25)_ to 47,000 for
_takeuchi(24, 9, 3)_. Optiscope successfully completes all 31 problem instances; BOHM exceeds the
100,000,000-node limit on the remaining 13.

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

[BOHM1.1]: https://github.com/asperti/BOHM1.1/tree/52d826aedbb00f0bd513d8bcbf2fc3fae1b758d2

## Optiscope Timings

On GNU/Linux, you need to reserve huge pages as follows: `sudo sysctl vm.nr_hugepages=6000`.

### [Ackermann function](ackermann.c)

Description: Computes the Ackermann function with initial values _(3, 8)_.

```
Benchmark 1: ./ackermann
  Time (mean ± σ):     715.6 ms ±   3.1 ms    [User: 706.9 ms, System: 7.1 ms]
  Range (min … max):   710.8 ms … 718.5 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 97530171
  Total interactions: 45984161
   Family reductions: 5571998
        Sharing work: 10.00%
    Bookkeeping work: 21.44%
             GC work: 45.70%
    Compression work: 2.86%
Max duplicator index: 2087945
 Max delimiter index: 0
     Peak node count: 23550
```

</details>

### [Takeuchi function](tak.c)

Description: Computes the Takeuchi function with initial values _(24, 9, 3)_.

```
Benchmark 1: ./tak
  Time (mean ± σ):     761.7 ms ±   3.1 ms    [User: 752.4 ms, System: 7.8 ms]
  Range (min … max):   758.1 ms … 765.0 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 101280294
  Total interactions: 48318661
   Family reductions: 4666911
        Sharing work: 9.98%
    Bookkeeping work: 31.14%
             GC work: 42.60%
    Compression work: 6.28%
Max duplicator index: 2017
 Max delimiter index: 0
     Peak node count: 1515
```

</details>

### [Scott list bubble sort](scott-bubble-sort.c)

Description: Performes a bubble sort on a Scott-encoded list of 300 cells, then sums all the cells
up.

```
Benchmark 1: ./scott-bubble-sort
  Time (mean ± σ):      1.748 s ±  0.006 s    [User: 1.733 s, System: 0.012 s]
  Range (min … max):    1.739 s …  1.754 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 212458817
  Total interactions: 208127135
   Family reductions: 633613
        Sharing work: 76.42%
    Bookkeeping work: 15.65%
             GC work: 0.98%
    Compression work: 0.36%
Max duplicator index: 5672
 Max delimiter index: 896
     Peak node count: 271337
```

</details>

### [Scott list insertion sort](scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 1000 cells, then sums all the
cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):     520.7 ms ±   4.2 ms    [User: 515.2 ms, System: 4.5 ms]
  Range (min … max):   518.2 ms … 528.1 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 55595551
  Total interactions: 23548520
   Family reductions: 4018010
        Sharing work: 3.60%
    Bookkeeping work: 42.33%
             GC work: 40.53%
    Compression work: 9.92%
Max duplicator index: 0
 Max delimiter index: 2
     Peak node count: 73135
```

</details>

### [Scott list merge sort](scott-merge-sort.c)

Description: Performes a merge sort on a Scott-encoded list of 1000 cells, then sums all the cells
up.

```
Benchmark 1: ./scott-merge-sort
  Time (mean ± σ):     248.9 ms ±   0.3 ms    [User: 244.9 ms, System: 3.0 ms]
  Range (min … max):   248.6 ms … 249.2 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 27135694
  Total interactions: 22075516
   Family reductions: 226408
        Sharing work: 28.45%
    Bookkeeping work: 63.04%
             GC work: 2.65%
    Compression work: 1.03%
Max duplicator index: 11676
 Max delimiter index: 5982
     Peak node count: 703375
```

</details>

### [Scott list quicksort](scott-quicksort.c)

Description: Performes a quicksort on a Scott-encoded list of 1000 cells, then sums all the cells
up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):      1.469 s ±  0.003 s    [User: 1.451 s, System: 0.014 s]
  Range (min … max):    1.465 s …  1.473 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 142615618
  Total interactions: 88015047
   Family reductions: 12020010
        Sharing work: 8.41%
    Bookkeeping work: 61.34%
             GC work: 14.80%
    Compression work: 11.57%
Max duplicator index: 8985
 Max delimiter index: 8976
     Peak node count: 3605397
```

</details>

### [N-queens](nqueens.c)

Description: Solves the 10-queens problem using Scott-encoded lists.

```
Benchmark 1: ./nqueens
  Time (mean ± σ):      3.308 s ±  0.015 s    [User: 3.273 s, System: 0.031 s]
  Range (min … max):    3.293 s …  3.326 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 377505855
  Total interactions: 216735189
   Family reductions: 16117939
        Sharing work: 24.65%
    Bookkeeping work: 35.56%
             GC work: 30.13%
    Compression work: 4.26%
Max duplicator index: 21
 Max delimiter index: 18
     Peak node count: 3004
```

</details>
