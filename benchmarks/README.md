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
  Time (mean ± σ):      1.235 s ±  0.020 s    [User: 1.226 s, System: 0.009 s]
  Range (min … max):    1.206 s …  1.256 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 96132115
Total interactions: 44586105
      Sharing work: 10.15%
  Bookkeeping work: 21.75%
           GC work: 46.37%
   Peak node count: 4182126
```

</details>

### [Takeuchi function](tak.c)

Description: Computes the Takeuchi function with initial values _(24, 9, 3)_.

```
Benchmark 1: ./tak
  Time (mean ± σ):      1.167 s ±  0.008 s    [User: 1.156 s, System: 0.010 s]
  Range (min … max):    1.157 s …  1.178 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 94492840
Total interactions: 41223349
      Sharing work: 14.96%
  Bookkeeping work: 30.87%
           GC work: 45.67%
   Peak node count: 3889561
```

</details>

### [Scott list bubble sort](scott-bubble-sort.c)

Description: Performes a bubble sort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-bubble-sort
  Time (mean ± σ):      2.912 s ±  0.025 s    [User: 2.908 s, System: 0.003 s]
  Range (min … max):    2.890 s …  2.950 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 198743125
Total interactions: 194765764
      Sharing work: 95.62%
  Bookkeeping work: 16.78%
           GC work: 1.05%
   Peak node count: 320355
```

</details>

### [Scott list insertion sort](scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):     858.0 ms ±   5.8 ms    [User: 853.1 ms, System: 4.6 ms]
  Range (min … max):   851.5 ms … 865.6 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 56067030
Total interactions: 24034015
      Sharing work: 5.35%
  Bookkeeping work: 45.52%
           GC work: 40.19%
   Peak node count: 1015215
```

</details>

### [Scott list merge sort](scott-merge-sort.c)

Description: Performes a merge sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-merge-sort
  Time (mean ± σ):     622.6 ms ±   6.4 ms    [User: 612.3 ms, System: 10.0 ms]
  Range (min … max):   614.8 ms … 629.9 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 41820676
Total interactions: 31013305
      Sharing work: 24.60%
  Bookkeeping work: 79.16%
           GC work: 1.74%
   Peak node count: 2215851
```

</details>

### [Scott list quicksort](scott-quicksort.c)

Description: Performes a quicksort on a Scott-encoded list of 500 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):      7.849 s ±  0.021 s    [User: 7.843 s, System: 0.005 s]
  Range (min … max):    7.814 s …  7.868 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 621743336
Total interactions: 440530792
      Sharing work: 0.56%
  Bookkeeping work: 98.00%
           GC work: 0.85%
   Peak node count: 1064779
```

</details>

### [N-queens](nqueens.c)

Description: Solves the 10-queens problem using Scott-encoded lists.

```
Benchmark 1: ./nqueens
  Time (mean ± σ):      5.325 s ±  0.274 s    [User: 5.318 s, System: 0.006 s]
  Range (min … max):    5.096 s …  5.757 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 379397882
Total interactions: 225243956
      Sharing work: 25.06%
  Bookkeeping work: 38.34%
           GC work: 29.04%
   Peak node count: 2383964
```

</details>
