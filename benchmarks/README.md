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
  Time (mean ± σ):      1.200 s ±  0.008 s    [User: 1.183 s, System: 0.016 s]
  Range (min … max):    1.191 s …  1.211 s    5 runs
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
        Sharing work: 10.15%
    Bookkeeping work: 18.85%
     Peak node count: 4182126
```

</details>

### [Takeuchi function](tak.c)

Description: Computes the Takeuchi function with initial values _(24, 9, 3)_.

```
Benchmark 1: ./tak
  Time (mean ± σ):      1.082 s ±  0.005 s    [User: 1.065 s, System: 0.017 s]
  Range (min … max):    1.075 s …  1.089 s    5 runs
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
        Sharing work: 14.96%
    Bookkeeping work: 28.90%
     Peak node count: 3889561
```

</details>

### [Scott list bubble sort](scott-bubble-sort.c)

Description: Performes a bubble sort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-bubble-sort
  Time (mean ± σ):      4.073 s ±  0.015 s    [User: 3.597 s, System: 0.475 s]
  Range (min … max):    4.065 s …  4.100 s    5 runs
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
        Sharing work: 95.68%
    Bookkeeping work: 16.68%
     Peak node count: 148549009
```

</details>

### [Scott list insertion sort](scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):     841.7 ms ±   7.5 ms    [User: 832.0 ms, System: 9.4 ms]
  Range (min … max):   829.2 ms … 847.9 ms    5 runs
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
        Sharing work: 5.35%
    Bookkeeping work: 42.84%
     Peak node count: 1014249
```

</details>

### [Scott list merge sort](scott-merge-sort.c)

Description: Performes a merge sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-merge-sort
  Time (mean ± σ):     619.0 ms ±   1.4 ms    [User: 593.6 ms, System: 25.3 ms]
  Range (min … max):   617.2 ms … 620.8 ms    5 runs
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
        Sharing work: 24.61%
    Bookkeeping work: 78.94%
     Peak node count: 7575569
```

</details>

### [Scott list quicksort](scott-quicksort.c)

Description: Performes a quicksort on a Scott-encoded list of 500 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):      6.819 s ±  0.023 s    [User: 6.812 s, System: 0.006 s]
  Range (min … max):    6.781 s …  6.842 s    5 runs
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
        Sharing work: 0.56%
    Bookkeeping work: 84.20%
     Peak node count: 1064615
```

</details>

### [N-queens](nqueens.c)

Description: Solves the 10-queens problem using Scott-encoded lists.

```
Benchmark 1: ./nqueens
  Time (mean ± σ):      4.607 s ±  0.017 s    [User: 4.585 s, System: 0.022 s]
  Range (min … max):    4.584 s …  4.624 s    5 runs
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
        Sharing work: 25.20%
    Bookkeeping work: 34.40%
     Peak node count: 6662066
```

</details>
