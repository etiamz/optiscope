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

### [Scott list bubble sort](scott-bubble-sort.c)

Description: Performes a bubble sort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-bubble-sort
  Time (mean ± σ):      4.584 s ±  0.013 s    [User: 4.073 s, System: 0.510 s]
  Range (min … max):    4.568 s …  4.599 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 632709
        Commutations: 151742280
       Annihilations: 27897016
          Expansions: 135752
     Cell operations: 271498
  Barrier operations: 14763760
  Total interactions: 195443015
 Garbage collections: 1585325
  Delimiter mergings: 362394
Delimiter extrusions: 1528812
      Total rewrites: 198919546
    Bookkeeping work: 9.55%
     Max duplicators: 269102
      Max delimiters: 13633506
     Max total nodes: 148546212
```

</details>

### [Scott list insertion sort](scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):     987.2 ms ±   6.3 ms    [User: 978.7 ms, System: 8.3 ms]
  Range (min … max):   979.8 ms … 997.1 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 4015006
        Commutations: 13513506
       Annihilations: 1498500
          Expansions: 1003003
     Cell operations: 1500500
  Barrier operations: 6514503
  Total interactions: 28045018
 Garbage collections: 8019015
  Delimiter mergings: 4500500
Delimiter extrusions: 4998999
      Total rewrites: 45563532
    Bookkeeping work: 42.82%
     Max duplicators: 3003
      Max delimiters: 4009
     Max total nodes: 503902
```

</details>

### [Scott list merge sort](scott-merge-sort.c)

Description: Performes a merge sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-merge-sort
  Time (mean ± σ):     853.0 ms ±   6.5 ms    [User: 819.2 ms, System: 33.5 ms]
  Range (min … max):   842.1 ms … 858.4 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 223404
        Commutations: 23740121
       Annihilations: 5501217
          Expansions: 40746
     Cell operations: 28931
  Barrier operations: 4207054
  Total interactions: 33741473
 Garbage collections: 468952
  Delimiter mergings: 180067
Delimiter extrusions: 9893731
      Total rewrites: 44284223
    Bookkeeping work: 66.80%
     Max duplicators: 18301
      Max delimiters: 1686648
     Max total nodes: 7564655
```

</details>

### [Scott list quicksort](scott-quicksort.c)

Description: Performes a quicksort on a Scott-encoded list of 500 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):     10.909 s ±  0.089 s    [User: 10.901 s, System: 0.007 s]
  Range (min … max):   10.808 s … 10.994 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 3008506
        Commutations: 345693273
       Annihilations: 86947260
          Expansions: 629252
     Cell operations: 749500
  Barrier operations: 88959265
  Total interactions: 525987056
 Garbage collections: 3792034
  Delimiter mergings: 2877244
Delimiter extrusions: 173031257
      Total rewrites: 705687591
    Bookkeeping work: 73.81%
     Max duplicators: 3006
      Max delimiters: 289503
     Max total nodes: 1054987
```

</details>
