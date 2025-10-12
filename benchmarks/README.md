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
  Time (mean ± σ):      1.189 s ±  0.017 s    [User: 1.179 s, System: 0.010 s]
  Range (min … max):    1.171 s …  1.217 s    5 runs
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
  Time (mean ± σ):      1.103 s ±  0.021 s    [User: 1.094 s, System: 0.009 s]
  Range (min … max):    1.083 s …  1.129 s    5 runs
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
  Time (mean ± σ):      2.425 s ±  0.040 s    [User: 2.421 s, System: 0.004 s]
  Range (min … max):    2.391 s …  2.493 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 632709
        Commutations: 151696832
       Annihilations: 27897016
          Expansions: 46052
     Cell operations: 271498
  Barrier operations: 14221657
  Total interactions: 194765764
 Garbage collections: 2086155
  Delimiter mergings: 362394
Delimiter extrusions: 1528812
      Total rewrites: 198743125
        Sharing work: 95.62%
    Bookkeeping work: 16.66%
     Peak node count: 320355
```

</details>

### [Scott list insertion sort](scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):     828.2 ms ±  15.3 ms    [User: 821.9 ms, System: 6.0 ms]
  Range (min … max):   814.1 ms … 851.8 ms    5 runs
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
     Peak node count: 1015215
```

</details>

### [Scott list merge sort](scott-merge-sort.c)

Description: Performes a merge sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-merge-sort
  Time (mean ± σ):     569.3 ms ±   5.0 ms    [User: 559.4 ms, System: 9.6 ms]
  Range (min … max):   564.6 ms … 577.6 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 223404
        Commutations: 23741973
       Annihilations: 5505101
          Expansions: 18907
     Cell operations: 28931
  Barrier operations: 1510973
  Total interactions: 31029289
 Garbage collections: 728577
  Delimiter mergings: 185063
Delimiter extrusions: 9893731
      Total rewrites: 41836660
        Sharing work: 24.63%
    Bookkeeping work: 78.94%
     Peak node count: 2215851
```

</details>

### [Scott list quicksort](scott-quicksort.c)

Description: Performes a quicksort on a Scott-encoded list of 500 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):      7.033 s ±  0.045 s    [User: 7.029 s, System: 0.004 s]
  Range (min … max):    6.982 s …  7.072 s    5 runs
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
     Peak node count: 1064779
```

</details>

### [N-queens](nqueens.c)

Description: Solves the 10-queens problem using Scott-encoded lists.

```
Benchmark 1: ./nqueens
  Time (mean ± σ):      4.870 s ±  0.064 s    [User: 4.865 s, System: 0.005 s]
  Range (min … max):    4.782 s …  4.930 s    5 runs
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
 Garbage collections: 110178876
  Delimiter mergings: 11649817
Delimiter extrusions: 32325233
      Total rewrites: 379397882
        Sharing work: 25.06%
    Bookkeeping work: 34.93%
     Peak node count: 2383964
```

</details>
