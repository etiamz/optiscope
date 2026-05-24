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
  Time (mean ± σ):      1.270 s ±  0.012 s    [User: 1.268 s, System: 0.002 s]
  Range (min … max):    1.256 s …  1.284 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 96134140
Total interactions: 44588130
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
  Time (mean ± σ):      1.290 s ±  0.008 s    [User: 1.288 s, System: 0.002 s]
  Range (min … max):    1.283 s …  1.305 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 96855837
Total interactions: 43894203
      Sharing work: 10.44%
  Bookkeeping work: 32.56%
           GC work: 44.55%
   Peak node count: 1514
```

</details>

### [Scott list bubble sort](scott-bubble-sort.c)

Description: Performes a bubble sort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-bubble-sort
  Time (mean ± σ):      2.823 s ±  0.067 s    [User: 2.819 s, System: 0.003 s]
  Range (min … max):    2.742 s …  2.894 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 198652588
Total interactions: 194587589
      Sharing work: 81.73%
  Bookkeeping work: 16.74%
           GC work: 1.05%
   Peak node count: 271338
```

</details>

### [Scott list insertion sort](scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):     887.5 ms ±   2.8 ms    [User: 885.7 ms, System: 1.6 ms]
  Range (min … max):   883.8 ms … 891.4 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 54093075
Total interactions: 23544536
      Sharing work: 3.70%
  Bookkeeping work: 43.50%
           GC work: 41.66%
   Peak node count: 73135
```

</details>

### [Scott list merge sort](scott-merge-sort.c)

Description: Performes a merge sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-merge-sort
  Time (mean ± σ):     591.4 ms ±   2.5 ms    [User: 581.7 ms, System: 9.5 ms]
  Range (min … max):   588.7 ms … 594.1 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 41766243
Total interactions: 30982806
      Sharing work: 18.48%
  Bookkeeping work: 79.06%
           GC work: 1.79%
   Peak node count: 2203877
```

</details>

### [Scott list quicksort](scott-quicksort.c)

Description: Performes a quicksort on a Scott-encoded list of 500 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):      2.547 s ±  0.010 s    [User: 2.535 s, System: 0.012 s]
  Range (min … max):    2.534 s …  2.559 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 152088334
Total interactions: 95496952
      Sharing work: 7.89%
  Bookkeeping work: 66.71%
           GC work: 14.54%
   Peak node count: 3605960
```

</details>

### [N-queens](nqueens.c)

Description: Solves the 10-queens problem using Scott-encoded lists.

```
Benchmark 1: ./nqueens
  Time (mean ± σ):      5.304 s ±  0.021 s    [User: 5.301 s, System: 0.002 s]
  Range (min … max):    5.283 s …  5.332 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 384625835
Total interactions: 223727257
      Sharing work: 24.20%
  Bookkeeping work: 37.70%
           GC work: 30.12%
   Peak node count: 10558
```

</details>
