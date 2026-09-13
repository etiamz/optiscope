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

To observe the performance characteristics of optimal reduction à la Lambdascope, we present a number of benchmarks that expose different computational patterns.

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

Description: Performes a bubble sort on a Scott-encoded list of 300 cells, then sums all the cells up.

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

Description: Performes an insertion sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

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

Description: Performes a merge sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

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

Description: Performes a quicksort on a Scott-encoded list of 1000 cells, then sums all the cells up.

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
