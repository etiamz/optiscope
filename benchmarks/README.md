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
  Time (mean ± σ):      1.211 s ±  0.047 s    [User: 1.208 s, System: 0.003 s]
  Range (min … max):    1.179 s …  1.295 s    5 runs
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
  Time (mean ± σ):      1.189 s ±  0.007 s    [User: 1.186 s, System: 0.003 s]
  Range (min … max):    1.181 s …  1.197 s    5 runs
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
  Time (mean ± σ):      2.852 s ±  0.043 s    [User: 2.848 s, System: 0.004 s]
  Range (min … max):    2.792 s …  2.908 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 198652587
Total interactions: 194632735
      Sharing work: 81.73%
  Bookkeeping work: 16.74%
           GC work: 1.05%
   Peak node count: 271335
```

</details>

### [Scott list insertion sort](scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):     893.2 ms ±   8.6 ms    [User: 890.4 ms, System: 2.4 ms]
  Range (min … max):   886.4 ms … 905.0 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 55092075
Total interactions: 23046036
      Sharing work: 3.63%
  Bookkeeping work: 44.53%
           GC work: 40.90%
   Peak node count: 72130
```

</details>

### [Scott list merge sort](scott-merge-sort.c)

Description: Performes a merge sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-merge-sort
  Time (mean ± σ):     691.3 ms ±   4.6 ms    [User: 682.2 ms, System: 9.0 ms]
  Range (min … max):   684.1 ms … 696.1 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 41744999
Total interactions: 30973479
      Sharing work: 18.49%
  Bookkeeping work: 79.09%
           GC work: 1.75%
   Peak node count: 2202603
```

</details>

### [Scott list quicksort](scott-quicksort.c)

Description: Performes a quicksort on a Scott-encoded list of 500 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):      2.569 s ±  0.021 s    [User: 2.557 s, System: 0.012 s]
  Range (min … max):    2.549 s …  2.605 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 147596830
Total interactions: 89010447
      Sharing work: 8.13%
  Bookkeeping work: 66.03%
           GC work: 14.64%
   Peak node count: 3410506
```

</details>

### [N-queens](nqueens.c)

Description: Solves the 10-queens problem using Scott-encoded lists.

```
Benchmark 1: ./nqueens
  Time (mean ± σ):      5.427 s ±  0.032 s    [User: 5.424 s, System: 0.003 s]
  Range (min … max):    5.384 s …  5.464 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 384976764
Total interactions: 228610169
      Sharing work: 24.17%
  Bookkeeping work: 38.83%
           GC work: 29.02%
   Peak node count: 10537
```

</details>
