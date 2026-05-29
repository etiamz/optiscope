# Benchmarks

<details>
<summary>System information</summary>

```
                          ./+o+-       etiamz@etiamz
                  yyyyy- -yyyyyy+      OS: Ubuntu 24.04 noble
               ://+//////-yyyyyyo      Kernel: x86_64 Linux 6.14.0-37-generic
           .++ .:/++++++/-.+sss/`      Uptime: 10m
         .:++o:  /++++++++/:--:/-      Packages: 1795
        o:+o+:++.`..```.-/oo+++++/     Shell: bash 5.2.21
       .:+o:+o/.          `+sssoo+/    Resolution: 3840x2400
  .++/+:+oo+o:`             /sssooo.   DE: GNOME 46.7
 /+++//+:`oo+o               /::--:.   WM: Mutter
 \+/+o+++`o++o               ++////.   WM Theme: Adwaita
  .++.o+++oo+:`             /dddhhh.   GTK Theme: Yaru-blue [GTK2/3]
       .+.o+oo:.          `oddhhhh+    Icon Theme: Yaru-blue
        \+.++o+o``-````.:ohdhhhhh+     Font: Ubuntu Sans 11
         `:o+++ `ohhhhhhhhyo++os:      Disk: 44G / 484G (10%)
           .o:`.syhhhhhhh/.oo++o`      CPU: AMD Ryzen 9 5900HX with Radeon Graphics @ 16x 4.683GHz
               /osyyyyyyo++ooo+++/     GPU: NVIDIA GeForce RTX 3050 Ti Laptop GPU
                   ````` +oo+++o\:     RAM: 2667MiB / 15388MiB
                          `oo++.      

```

</details>

To observe the performance characteristics of optimal reduction à la Lambdascope, we present a number of benchmarks that expose different computational patterns.

On GNU/Linux, you need to reserve huge pages as follows: `sudo sysctl vm.nr_hugepages=6000`.

### [Ackermann function](ackermann.c)

Description: Computes the Ackermann function with initial values _(3, 8)_.

```
Benchmark 1: ./ackermann
  Time (mean ± σ):      1.208 s ±  0.007 s    [User: 1.207 s, System: 0.001 s]
  Range (min … max):    1.199 s …  1.216 s    5 runs
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
  Time (mean ± σ):      1.274 s ±  0.018 s    [User: 1.271 s, System: 0.002 s]
  Range (min … max):    1.252 s …  1.300 s    5 runs
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
  Time (mean ± σ):      2.805 s ±  0.038 s    [User: 2.801 s, System: 0.004 s]
  Range (min … max):    2.767 s …  2.867 s    5 runs
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
  Time (mean ± σ):     916.6 ms ±   4.3 ms    [User: 914.7 ms, System: 1.8 ms]
  Range (min … max):   910.8 ms … 921.7 ms    5 runs
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
  Time (mean ± σ):     599.2 ms ±   3.2 ms    [User: 590.5 ms, System: 8.5 ms]
  Range (min … max):   596.4 ms … 604.1 ms    5 runs
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
  Time (mean ± σ):      2.602 s ±  0.023 s    [User: 2.590 s, System: 0.012 s]
  Range (min … max):    2.580 s …  2.637 s    5 runs
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
  Time (mean ± σ):      5.118 s ±  0.062 s    [User: 5.114 s, System: 0.003 s]
  Range (min … max):    5.043 s …  5.181 s    5 runs
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
