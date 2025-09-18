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
  Time (mean ± σ):      1.334 s ±  0.017 s    [User: 1.326 s, System: 0.007 s]
  Range (min … max):    1.316 s …  1.355 s    5 runs
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
  Time (mean ± σ):      1.327 s ±  0.026 s    [User: 1.318 s, System: 0.009 s]
  Range (min … max):    1.295 s …  1.364 s    5 runs
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
  Time (mean ± σ):      4.621 s ±  0.046 s    [User: 4.139 s, System: 0.481 s]
  Range (min … max):    4.566 s …  4.682 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 632709
        Commutations: 151741682
       Annihilations: 27897016
          Expansions: 46052
     Cell operations: 271498
  Barrier operations: 14221657
  Total interactions: 194810614
 Garbage collections: 1906453
  Delimiter mergings: 362394
Delimiter extrusions: 1528812
      Total rewrites: 198608273
    Bookkeeping work: 9.72%
     Max duplicators: 269102
      Max delimiters: 13633506
     Max total nodes: 148546215
```

</details>

### [Scott list insertion sort](scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):     924.9 ms ±  16.0 ms    [User: 921.5 ms, System: 3.1 ms]
  Range (min … max):   910.4 ms … 949.6 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 4015006
        Commutations: 12008007
       Annihilations: 1498500
          Expansions: 502502
     Cell operations: 1500500
  Barrier operations: 4509500
  Total interactions: 24034015
 Garbage collections: 22531515
  Delimiter mergings: 4502501
Delimiter extrusions: 4998999
      Total rewrites: 56067030
    Bookkeeping work: 40.16%
     Max duplicators: 6000
      Max delimiters: 18989
     Max total nodes: 1005176
```

</details>

### [Scott list merge sort](scott-merge-sort.c)

Description: Performes a merge sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-merge-sort
  Time (mean ± σ):     728.9 ms ±   6.5 ms    [User: 703.9 ms, System: 24.9 ms]
  Range (min … max):   717.6 ms … 733.7 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 223404
        Commutations: 23750874
       Annihilations: 5503103
          Expansions: 18907
     Cell operations: 28931
  Barrier operations: 1510973
  Total interactions: 31036192
 Garbage collections: 687208
  Delimiter mergings: 185063
Delimiter extrusions: 9893731
      Total rewrites: 41802194
    Bookkeeping work: 75.74%
     Max duplicators: 18301
      Max delimiters: 2194209
     Max total nodes: 7566430
```

</details>

### [Scott list quicksort](scott-quicksort.c)

Description: Performes a quicksort on a Scott-encoded list of 500 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):      8.161 s ±  0.073 s    [User: 8.156 s, System: 0.004 s]
  Range (min … max):    8.090 s …  8.282 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 3008506
        Commutations: 345694771
       Annihilations: 86947759
          Expansions: 377252
     Cell operations: 749500
  Barrier operations: 3753004
  Total interactions: 440530792
 Garbage collections: 5304042
  Delimiter mergings: 2877245
Delimiter extrusions: 173031257
      Total rewrites: 621743336
    Bookkeeping work: 83.97%
     Max duplicators: 3006
      Max delimiters: 290663
     Max total nodes: 1060027
```

</details>

### [N-queens](nqueens.c)

Description: Solves the 10-queens problem using Scott-encoded lists.

```
Benchmark 1: ./nqueens
  Time (mean ± σ):      5.338 s ±  0.082 s    [User: 5.318 s, System: 0.019 s]
  Range (min … max):    5.217 s …  5.438 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 16117939
        Commutations: 147016880
       Annihilations: 29144059
          Expansions: 2899293
     Cell operations: 11692140
  Barrier operations: 18373645
  Total interactions: 225243956
 Garbage collections: 108055973
  Delimiter mergings: 11649817
Delimiter extrusions: 32325233
      Total rewrites: 377274979
    Bookkeeping work: 32.90%
     Max duplicators: 36466
      Max delimiters: 217164
     Max total nodes: 6661915
```

</details>
