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
  Time (mean ± σ):      1.196 s ±  0.004 s    [User: 1.193 s, System: 0.003 s]
  Range (min … max):    1.188 s …  1.200 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 96132115
Total interactions: 44586105
      Sharing work: 10.14%
  Bookkeeping work: 21.75%
           GC work: 46.37%
   Peak node count: 23549
```

</details>

### [Takeuchi function](tak.c)

Description: Computes the Takeuchi function with initial values _(24, 9, 3)_.

```
Benchmark 1: ./tak
  Time (mean ± σ):      1.197 s ±  0.017 s    [User: 1.195 s, System: 0.002 s]
  Range (min … max):    1.178 s …  1.222 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 93812330
Total interactions: 41057777
      Sharing work: 10.78%
  Bookkeeping work: 30.37%
           GC work: 46.00%
   Peak node count: 1481
```

</details>

### [Scott list bubble sort](scott-bubble-sort.c)

Description: Performes a bubble sort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-bubble-sort
  Time (mean ± σ):      2.829 s ±  0.020 s    [User: 2.823 s, System: 0.005 s]
  Range (min … max):    2.806 s …  2.857 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 198785032
Total interactions: 194362130
      Sharing work: 81.68%
  Bookkeeping work: 16.82%
           GC work: 1.03%
   Peak node count: 271334
```

</details>

### [Scott list insertion sort](scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):     896.8 ms ±   9.4 ms    [User: 893.7 ms, System: 2.9 ms]
  Range (min … max):   885.3 ms … 904.8 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 54592575
Total interactions: 22047036
      Sharing work: 3.67%
  Bookkeeping work: 44.02%
           GC work: 41.27%
   Peak node count: 72130
```

</details>

### [Scott list merge sort](scott-merge-sort.c)

Description: Performes a merge sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-merge-sort
  Time (mean ± σ):     420.7 ms ±   0.6 ms    [User: 416.8 ms, System: 3.7 ms]
  Range (min … max):   420.0 ms … 421.5 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 25876230
Total interactions: 20774470
      Sharing work: 29.83%
  Bookkeeping work: 66.39%
           GC work: 2.70%
   Peak node count: 703361
```

</details>

### [Scott list quicksort](scott-quicksort.c)

Description: Performes a quicksort on a Scott-encoded list of 500 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):      2.374 s ±  0.040 s    [User: 2.361 s, System: 0.012 s]
  Range (min … max):    2.348 s …  2.444 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 139111130
Total interactions: 82513554
      Sharing work: 8.62%
  Bookkeeping work: 64.32%
           GC work: 15.17%
   Peak node count: 3102284
```

</details>

### [N-queens](nqueens.c)

Description: Solves the 10-queens problem using Scott-encoded lists.

```
Benchmark 1: ./nqueens
  Time (mean ± σ):      5.433 s ±  0.101 s    [User: 5.430 s, System: 0.002 s]
  Range (min … max):    5.340 s …  5.592 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 377040456
Total interactions: 219616494
      Sharing work: 24.68%
  Bookkeeping work: 38.01%
           GC work: 29.16%
   Peak node count: 2728
```

</details>
