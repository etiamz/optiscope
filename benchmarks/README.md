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
| _bsort(25)_ | 138,599 | 286,806 | 2,887 | 99,580 |
| _bsort(50)_ | 922,149 | 2,096,756 | 7,587 | 736,205 |
| _bsort(150)_ | 21,756,349 | 53,317,806 | 67,338 | 18,970,205 |
| _bsort(300)_ | 167,982,649 | - | 269,538 | >100,000,000 |
| _isort(50)_ | 122,818 | 1,460,019 | 3,936 | 216,223 |
| _isort(100)_ | 478,043 | 10,994,969 | 7,736 | 1,692,023 |
| _isort(500)_ | 11,689,843 | - | 38,136 | >100,000,000 |
| _isort(1000)_ | 46,629,593 | - | 76,136 | >100,000,000 |
| _msort(50)_ | 81,311 | 2,044,194 | 1,811 | 1,150,810 |
| _msort(100)_ | 217,817 | 13,490,459 | 3,104 | 9,202,565 |
| _msort(500)_ | 2,793,241 | - | 13,199 | >100,000,000 |
| _msort(1000)_ | 9,513,027 | - | 25,706 | >100,000,000 |
| _qsort(50)_ | 245,319 | 3,216,791 | 10,657 | 470,476 |
| _qsort(100)_ | 960,519 | 24,096,191 | 33,632 | 3,368,151 |
| _qsort(500)_ | 23,602,119 | - | 698,821 | >100,000,000 |
| _qsort(1000)_ | 94,204,119 | - | 2,758,460 | >100,000,000 |
| _nqueens(5)_ | 126,943 | 577,712 | 671 | 483,248 |
| _nqueens(6)_ | 561,123 | 3,558,354 | 769 | 3,155,771 |
| _nqueens(7)_ | 2,497,195 | 22,250,541 | 903 | 20,746,272 |
| _nqueens(8)_ | 11,937,901 | - | 1,087 | >100,000,000 |
| _nqueens(9)_ | 59,626,035 | - | 1,488 | >100,000,000 |
| _nqueens(10)_ | 308,598,243 | - | 2,067 | >100,000,000 |
| _nqueens(11)_ | 1,718,454,579 | - | 4,399 | >100,000,000 |

Among the problem instances completed by both reducers, Optiscope reduces total graph rewrites by
factors of approximately 1.3 for Ackermann, 2.1-2.5 for bubble sort, 12-23 for insertion sort, 25-62
for merge sort, 13-25 for quicksort, & 4.6-8.9 for N-queens. On _takeuchi(24, 7, 3)_, Optiscope
requires approximately 1.3% more rewrites than BOHM, but 0.9% & 2.9% fewer on _takeuchi(24, 8, 3)_ &
_takeuchi(24, 9, 3)_, respectively. Optiscope's peak node counts are lower in every completed
comparison, by factors ranging from approximately 34 for _bsort(25)_ to 49,000 for _takeuchi(24, 9,
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

### [Ackermann function](ackermann.c)

Description: Computes the Ackermann function with initial values _(3, 8)_.

```
Benchmark 1: ./ackermann
  Time (mean ± σ):     925.5 ms ±   3.9 ms    [User: 918.0 ms, System: 5.8 ms]
  Range (min … max):   922.4 ms … 931.3 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 118408009
  Total interactions: 50152048
   Family reductions: 5571998
        Sharing work: 8.24%
    Bookkeeping work: 21.18%
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
  Time (mean ± σ):     694.0 ms ±   4.2 ms    [User: 688.8 ms, System: 3.8 ms]
  Range (min … max):   689.7 ms … 700.2 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 90615854
  Total interactions: 38890919
   Family reductions: 4666911
        Sharing work: 11.16%
    Bookkeeping work: 17.17%
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
  Time (mean ± σ):      1.354 s ±  0.010 s    [User: 1.346 s, System: 0.006 s]
  Range (min … max):    1.342 s …  1.363 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 167982649
  Total interactions: 164895643
   Family reductions: 633613
        Sharing work: 96.65%
    Bookkeeping work: 0.94%
             GC work: 1.14%
    Compression work: 0.00%
Max duplicator index: 602
 Max delimiter index: 0
     Peak node count: 269538
```

</details>

### [Scott list insertion sort](scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 1000 cells, then sums all the
cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):     459.6 ms ±   0.9 ms    [User: 456.6 ms, System: 2.1 ms]
  Range (min … max):   458.3 ms … 460.7 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 46629593
  Total interactions: 16051036
   Family reductions: 4018010
        Sharing work: 4.29%
    Bookkeeping work: 22.62%
             GC work: 47.26%
    Compression work: 0.00%
Max duplicator index: 0
 Max delimiter index: 0
     Peak node count: 76136
```

</details>

### [Scott list merge sort](scott-merge-sort.c)

Description: Performes a merge sort on a Scott-encoded list of 1000 cells, then sums all the cells
up.

```
Benchmark 1: ./scott-merge-sort
  Time (mean ± σ):      86.3 ms ±   0.4 ms    [User: 84.7 ms, System: 1.0 ms]
  Range (min … max):    85.7 ms …  86.7 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 9513027
  Total interactions: 8404394
   Family reductions: 226408
        Sharing work: 81.14%
    Bookkeeping work: 5.30%
             GC work: 7.17%
    Compression work: 0.00%
Max duplicator index: 2002
 Max delimiter index: 0
     Peak node count: 25706
```

</details>

### [Scott list quicksort](scott-quicksort.c)

Description: Performes a quicksort on a Scott-encoded list of 1000 cells, then sums all the cells
up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):     985.6 ms ±   4.0 ms    [User: 975.1 ms, System: 7.9 ms]
  Range (min … max):   982.1 ms … 992.4 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 94204119
  Total interactions: 49050035
   Family reductions: 12020010
        Sharing work: 12.73%
    Bookkeeping work: 30.30%
             GC work: 21.35%
    Compression work: 0.00%
Max duplicator index: 2000
 Max delimiter index: 0
     Peak node count: 2758460
```

</details>

### [N-queens](nqueens.c)

Description: Solves the 10-queens problem using Scott-encoded lists.

```
Benchmark 1: ./nqueens
  Time (mean ± σ):      2.616 s ±  0.003 s    [User: 2.601 s, System: 0.012 s]
  Range (min … max):    2.612 s …  2.619 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 308598243
  Total interactions: 159905900
   Family reductions: 16117939
        Sharing work: 30.16%
    Bookkeeping work: 14.29%
             GC work: 36.87%
    Compression work: 0.00%
Max duplicator index: 20
 Max delimiter index: 0
     Peak node count: 2067
```

</details>
