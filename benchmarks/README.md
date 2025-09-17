# Benchmarks

<details>
<summary>System information</summary>

```
                          ./+o+-       etiamz@etiamz
                  yyyyy- -yyyyyy+      OS: Ubuntu 24.04 noble
               ://+//////-yyyyyyo      Kernel: x86_64 Linux 6.8.0-60-generic
           .++ .:/++++++/-.+sss/`      Uptime: 16m
         .:++o:  /++++++++/:--:/-      Packages: 2799
        o:+o+:++.`..```.-/oo+++++/     Shell: bash 5.2.21
       .:+o:+o/.          `+sssoo+/    Resolution: 3840x2400
  .++/+:+oo+o:`             /sssooo.   DE: GNOME 46.7
 /+++//+:`oo+o               /::--:.   WM: Mutter
 \+/+o+++`o++o               ++////.   WM Theme: Adwaita
  .++.o+++oo+:`             /dddhhh.   GTK Theme: Yaru-red [GTK2/3]
       .+.o+oo:.          `oddhhhh+    Icon Theme: Yaru-red
        \+.++o+o``-````.:ohdhhhhh+     Font: Ubuntu Sans Bold 11 @wght=700
         `:o+++ `ohhhhhhhhyo++os:      Disk: 389G / 484G (85%)
           .o:`.syhhhhhhh/.oo++o`      CPU: AMD Ryzen 9 5900HX with Radeon Graphics @ 16x 4.68GHz
               /osyyyyyyo++ooo+++/     GPU: AMD/ATI Cezanne [Radeon Vega Series / Radeon Vega Mobile Series]
                   ````` +oo+++o\:     RAM: 5849MiB / 15388MiB
                          `oo++.
```

</details>

To observe the performance characteristics of optimal reduction à la Lambdascope, we present a number of benchmarks that expose different computational patterns.

On GNU/Linux, you need to reserve huge pages as follows: `sudo sysctl vm.nr_hugepages=6000`.

### [Ackermann function](ackermann.c)

Description: Computes the Ackermann function with initial values _(3, 8)_.

```
Benchmark 1: ./ackermann
  Time (mean ± σ):      1.283 s ±  0.011 s    [User: 1.269 s, System: 0.013 s]
  Range (min … max):    1.273 s …  1.300 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 5571998
        Commutations: 19508073
       Annihilations: 2025
          Expansions: 2785999
     Cell operations: 12538012
  Barrier operations: 4179998
  Total interactions: 44586105
 Garbage collections: 44575980
  Delimiter mergings: 2788012
Delimiter extrusions: 4182018
      Total rewrites: 96132115
    Bookkeeping work: 17.40%
     Max duplicators: 1026
      Max delimiters: 3071
     Max total nodes: 4182102
```

</details>

### [Takeuchi function](tak.c)

Description: Computes the Takeuchi function with initial values _(24, 9, 3)_.

```
Benchmark 1: ./tak
  Time (mean ± σ):      1.340 s ±  0.024 s    [User: 1.325 s, System: 0.015 s]
  Range (min … max):    1.316 s …  1.366 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 4666911
        Commutations: 22878891
       Annihilations: 1780801
          Expansions: 1555637
     Cell operations: 5833638
  Barrier operations: 4507471
  Total interactions: 41223349
 Garbage collections: 43150388
  Delimiter mergings: 6326998
Delimiter extrusions: 3792105
      Total rewrites: 94492840
    Bookkeeping work: 27.44%
     Max duplicators: 181
      Max delimiters: 363
     Max total nodes: 3889531
```

</details>

### [Scott list bubble sort](scott-bubble-sort.c)

Description: Performes a bubble sort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-bubble-sort
  Time (mean ± σ):      4.076 s ±  0.008 s    [User: 3.589 s, System: 0.486 s]
  Range (min … max):    4.070 s …  4.085 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 632709
        Commutations: 151741683
       Annihilations: 27897016
          Expansions: 135752
     Cell operations: 271498
  Barrier operations: 14221657
  Total interactions: 194900315
 Garbage collections: 1586221
  Delimiter mergings: 362394
Delimiter extrusions: 1528812
      Total rewrites: 198377742
    Bookkeeping work: 9.62%
     Max duplicators: 269102
      Max delimiters: 13633506
     Max total nodes: 148546215
```

</details>

### [Scott list insertion sort](scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):     585.6 ms ±  12.6 ms    [User: 577.3 ms, System: 8.1 ms]
  Range (min … max):   572.5 ms … 604.3 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 4015006
        Commutations: 12009008
       Annihilations: 1498500
          Expansions: 1003003
     Cell operations: 1500500
  Barrier operations: 4509500
  Total interactions: 24535517
 Garbage collections: 9021015
  Delimiter mergings: 4502500
Delimiter extrusions: 4998999
      Total rewrites: 43058031
    Bookkeeping work: 51.13%
     Max duplicators: 6000
      Max delimiters: 9998
     Max total nodes: 1004309
```

</details>

### [Scott list merge sort](scott-merge-sort.c)

Description: Performes a merge sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-merge-sort
  Time (mean ± σ):     711.3 ms ±   1.5 ms    [User: 678.2 ms, System: 32.8 ms]
  Range (min … max):   709.3 ms … 712.9 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 223404
        Commutations: 23751210
       Annihilations: 5501217
          Expansions: 40746
     Cell operations: 28931
  Barrier operations: 1510973
  Total interactions: 31056481
 Garbage collections: 468952
  Delimiter mergings: 181954
Delimiter extrusions: 9893731
      Total rewrites: 41601118
    Bookkeeping work: 72.38%
     Max duplicators: 18301
      Max delimiters: 2192155
     Max total nodes: 7562658
```

</details>

### [Scott list quicksort](scott-quicksort.c)

Description: Performes a quicksort on a Scott-encoded list of 500 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):      7.997 s ±  0.035 s    [User: 7.989 s, System: 0.008 s]
  Range (min … max):    7.953 s …  8.051 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 3008506
        Commutations: 345818524
       Annihilations: 86947260
          Expansions: 629252
     Cell operations: 749500
  Barrier operations: 3753004
  Total interactions: 440906046
 Garbage collections: 3792034
  Delimiter mergings: 2877245
Delimiter extrusions: 173031257
      Total rewrites: 620606582
    Bookkeeping work: 83.99%
     Max duplicators: 3006
      Max delimiters: 289505
     Max total nodes: 1054653
```

</details>

### [N-queens](nqueens.c)

Description: Solves the 10-queens problem using Scott-encoded lists.

```
Benchmark 1: ./nqueens
  Time (mean ± σ):      4.919 s ±  0.032 s    [User: 4.898 s, System: 0.020 s]
  Range (min … max):    4.884 s …  4.967 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 16117939
        Commutations: 145688549
       Annihilations: 28099944
          Expansions: 3158690
     Cell operations: 11692140
  Barrier operations: 18373645
  Total interactions: 223130907
 Garbage collections: 96931927
  Delimiter mergings: 11461070
Delimiter extrusions: 32325233
      Total rewrites: 363849137
    Bookkeeping work: 33.40%
     Max duplicators: 19141
      Max delimiters: 20168
     Max total nodes: 6221044
```

</details>
