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
  Time (mean ± σ):      1.257 s ±  0.004 s    [User: 1.254 s, System: 0.003 s]
  Range (min … max):    1.252 s …  1.262 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 96134140
  Total interactions: 44588130
        Sharing work: 10.14%
    Bookkeeping work: 21.75%
             GC work: 46.37%
    Compression work: 2.90%
Max duplicator index: 2087945
 Max delimiter index: 0
     Peak node count: 23549
```

</details>

### [Takeuchi function](tak.c)

Description: Computes the Takeuchi function with initial values _(24, 9, 3)_.

```
Benchmark 1: ./tak
  Time (mean ± σ):      1.352 s ±  0.076 s    [User: 1.349 s, System: 0.003 s]
  Range (min … max):    1.297 s …  1.484 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 96855837
  Total interactions: 43894203
        Sharing work: 10.44%
    Bookkeeping work: 32.56%
             GC work: 44.55%
    Compression work: 6.57%
Max duplicator index: 2017
 Max delimiter index: 0
     Peak node count: 1514
```

</details>

### [Scott list bubble sort](scott-bubble-sort.c)

Description: Performes a bubble sort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-bubble-sort
  Time (mean ± σ):      2.806 s ±  0.071 s    [User: 2.801 s, System: 0.005 s]
  Range (min … max):    2.740 s …  2.909 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 198652284
  Total interactions: 194586986
        Sharing work: 81.73%
    Bookkeeping work: 16.74%
             GC work: 1.05%
    Compression work: 0.25%
Max duplicator index: 5673
 Max delimiter index: 1502
     Peak node count: 271338
```

</details>

### [Scott list insertion sort](scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):     902.0 ms ±   5.0 ms    [User: 899.7 ms, System: 1.9 ms]
  Range (min … max):   895.1 ms … 907.4 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 54093075
  Total interactions: 23544536
        Sharing work: 3.70%
    Bookkeeping work: 43.50%
             GC work: 41.66%
    Compression work: 7.42%
Max duplicator index: 0
 Max delimiter index: 2
     Peak node count: 73135
```

</details>

### [Scott list merge sort](scott-merge-sort.c)

Description: Performes a merge sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-merge-sort
  Time (mean ± σ):     602.8 ms ±   3.9 ms    [User: 593.2 ms, System: 9.4 ms]
  Range (min … max):   597.1 ms … 607.0 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 41766243
  Total interactions: 30982806
        Sharing work: 18.48%
    Bookkeeping work: 79.06%
             GC work: 1.79%
    Compression work: 0.41%
Max duplicator index: 1611588
 Max delimiter index: 1611585
     Peak node count: 2203877
```

</details>

### [Scott list quicksort](scott-quicksort.c)

Description: Performes a quicksort on a Scott-encoded list of 500 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):      2.586 s ±  0.007 s    [User: 2.574 s, System: 0.012 s]
  Range (min … max):    2.578 s …  2.597 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 152088334
  Total interactions: 95496952
        Sharing work: 7.89%
    Bookkeeping work: 66.71%
             GC work: 14.54%
    Compression work: 8.22%
Max duplicator index: 8986
 Max delimiter index: 8989
     Peak node count: 3605960
```

</details>

### [N-queens](nqueens.c)

Description: Solves the 10-queens problem using Scott-encoded lists.

```
Benchmark 1: ./nqueens
  Time (mean ± σ):      5.322 s ±  0.066 s    [User: 5.317 s, System: 0.004 s]
  Range (min … max):    5.254 s …  5.425 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
      Total rewrites: 380246509
  Total interactions: 221012523
        Sharing work: 24.48%
    Bookkeeping work: 37.13%
             GC work: 30.32%
    Compression work: 2.81%
Max duplicator index: 21
 Max delimiter index: 500931
     Peak node count: 10558
```

</details>
