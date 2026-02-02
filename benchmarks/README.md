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
  Time (mean ± σ):      1.227 s ±  0.010 s    [User: 1.218 s, System: 0.008 s]
  Range (min … max):    1.215 s …  1.242 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 96132115
Total interactions: 44586105
      Sharing work: 10.14%
  Bookkeeping work: 21.75%
           GC work: 46.37%
   Peak node count: 4182126
```

</details>

### [Takeuchi function](tak.c)

Description: Computes the Takeuchi function with initial values _(24, 9, 3)_.

```
Benchmark 1: ./tak
  Time (mean ± σ):      1.104 s ±  0.005 s    [User: 1.092 s, System: 0.012 s]
  Range (min … max):    1.099 s …  1.111 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 93812330
Total interactions: 41057777
      Sharing work: 10.78%
  Bookkeeping work: 30.37%
           GC work: 46.00%
   Peak node count: 3889561
```

</details>

### [Scott list bubble sort](scott-bubble-sort.c)

Description: Performes a bubble sort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-bubble-sort
  Time (mean ± σ):      2.755 s ±  0.015 s    [User: 2.750 s, System: 0.004 s]
  Range (min … max):    2.741 s …  2.780 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 198647435
Total interactions: 194628809
      Sharing work: 81.73%
  Bookkeeping work: 16.74%
           GC work: 1.05%
   Peak node count: 320354
```

</details>

### [Scott list insertion sort](scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):     861.6 ms ±   7.3 ms    [User: 855.8 ms, System: 5.5 ms]
  Range (min … max):   854.0 ms … 869.5 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 55068028
Total interactions: 23029015
      Sharing work: 3.63%
  Bookkeeping work: 44.53%
           GC work: 40.92%
   Peak node count: 1015215
```

</details>

### [Scott list merge sort](scott-merge-sort.c)

Description: Performes a merge sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-merge-sort
  Time (mean ± σ):     619.9 ms ±   2.4 ms    [User: 609.7 ms, System: 9.9 ms]
  Range (min … max):   616.7 ms … 622.9 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 41734945
Total interactions: 30966452
      Sharing work: 18.49%
  Bookkeeping work: 79.12%
           GC work: 1.75%
   Peak node count: 2215588
```

</details>

### [Scott list quicksort](scott-quicksort.c)

Description: Performes a quicksort on a Scott-encoded list of 500 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):      4.120 s ±  0.011 s    [User: 4.114 s, System: 0.005 s]
  Range (min … max):    4.102 s …  4.131 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 329952069
Total interactions: 231451768
      Sharing work: 0.91%
  Bookkeeping work: 96.23%
           GC work: 1.61%
   Peak node count: 1064132
```

</details>

### [N-queens](nqueens.c)

Description: Solves the 10-queens problem using Scott-encoded lists.

```
Benchmark 1: ./nqueens
  Time (mean ± σ):      5.141 s ±  0.022 s    [User: 5.135 s, System: 0.006 s]
  Range (min … max):    5.117 s …  5.172 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
    Total rewrites: 377594618
Total interactions: 222839462
      Sharing work: 24.65%
  Bookkeeping work: 38.04%
           GC work: 29.18%
   Peak node count: 2383964
```

</details>
