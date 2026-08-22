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
  Time (mean ± σ):     756.1 ms ±   6.3 ms    [User: 751.0 ms, System: 3.9 ms]
  Range (min … max):   747.5 ms … 762.6 ms    5 runs
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
  Time (mean ± σ):     798.0 ms ±   6.2 ms    [User: 790.7 ms, System: 5.6 ms]
  Range (min … max):   790.3 ms … 806.7 ms    5 runs
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
  Time (mean ± σ):      1.804 s ±  0.035 s    [User: 1.786 s, System: 0.013 s]
  Range (min … max):    1.774 s …  1.841 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 212458817
  Total interactions: 208396235
   Family reductions: 633613
        Sharing work: 76.42%
    Bookkeeping work: 15.65%
             GC work: 0.98%
    Compression work: 0.23%
Max duplicator index: 5672
 Max delimiter index: 896
     Peak node count: 271337
```

</details>

### [Scott list insertion sort](scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):     534.4 ms ±   1.0 ms    [User: 529.9 ms, System: 3.5 ms]
  Range (min … max):   533.2 ms … 535.9 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 55595551
  Total interactions: 25047020
   Family reductions: 4018010
        Sharing work: 3.60%
    Bookkeeping work: 42.33%
             GC work: 40.53%
    Compression work: 7.22%
Max duplicator index: 0
 Max delimiter index: 2
     Peak node count: 73135
```

</details>

### [Scott list merge sort](scott-merge-sort.c)

Description: Performes a merge sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-merge-sort
  Time (mean ± σ):     439.3 ms ±  14.8 ms    [User: 430.1 ms, System: 7.5 ms]
  Range (min … max):   424.7 ms … 458.1 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 43090777
  Total interactions: 32311354
   Family reductions: 226408
        Sharing work: 17.91%
    Bookkeeping work: 76.60%
             GC work: 1.73%
    Compression work: 0.40%
Max duplicator index: 1611588
 Max delimiter index: 1611585
     Peak node count: 2201878
```

</details>

### [Scott list quicksort](scott-quicksort.c)

Description: Performes a quicksort on a Scott-encoded list of 500 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):      1.761 s ±  0.017 s    [User: 1.740 s, System: 0.016 s]
  Range (min … max):    1.732 s …  1.775 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 157093318
  Total interactions: 100501942
   Family reductions: 12020010
        Sharing work: 7.64%
    Bookkeeping work: 64.58%
             GC work: 14.08%
    Compression work: 7.96%
Max duplicator index: 8985
 Max delimiter index: 8988
     Peak node count: 3605962
```

</details>

### [N-queens](nqueens.c)

Description: Solves the 10-queens problem using Scott-encoded lists.

```
Benchmark 1: ./nqueens
  Time (mean ± σ):      3.486 s ±  0.036 s    [User: 3.457 s, System: 0.024 s]
  Range (min … max):    3.455 s …  3.542 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 379906038
  Total interactions: 223567517
   Family reductions: 16117939
        Sharing work: 24.50%
    Bookkeeping work: 35.84%
             GC work: 30.07%
    Compression work: 2.75%
Max duplicator index: 21
 Max delimiter index: 406932
     Peak node count: 7391
```

</details>
