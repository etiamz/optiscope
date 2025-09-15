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
  Time (mean ± σ):      4.574 s ±  0.033 s    [User: 3.979 s, System: 0.592 s]
  Range (min … max):    4.534 s …  4.616 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 632709
        Commutations: 151742283
       Annihilations: 27897016
          Expansions: 135752
     Cell operations: 271498
  Barrier operations: 14221657
  Total interactions: 194900915
 Garbage collections: 1585325
  Delimiter mergings: 362394
Delimiter extrusions: 1528812
      Total rewrites: 198377446
    Bookkeeping work: 9.57%
     Max duplicators: 269102
      Max delimiters: 13633506
     Max total nodes: 148545611
```

</details>

### [Scott list insertion sort](scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):     903.3 ms ±  12.0 ms    [User: 896.1 ms, System: 6.9 ms]
  Range (min … max):   889.7 ms … 919.2 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 4015006
        Commutations: 13513509
       Annihilations: 1498500
          Expansions: 1003003
     Cell operations: 1500500
  Barrier operations: 4509498
  Total interactions: 26040016
 Garbage collections: 8019015
  Delimiter mergings: 4500500
Delimiter extrusions: 4998999
      Total rewrites: 43558530
    Bookkeeping work: 44.79%
     Max duplicators: 3003
      Max delimiters: 4009
     Max total nodes: 501958
```

</details>

### [Scott list merge sort](scott-merge-sort.c)

Description: Performes a merge sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-merge-sort
  Time (mean ± σ):     769.2 ms ±   5.2 ms    [User: 738.4 ms, System: 30.7 ms]
  Range (min … max):   762.7 ms … 775.7 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 223404
        Commutations: 23740124
       Annihilations: 5501217
          Expansions: 40746
     Cell operations: 28931
  Barrier operations: 1510973
  Total interactions: 31045395
 Garbage collections: 468952
  Delimiter mergings: 180067
Delimiter extrusions: 9893731
      Total rewrites: 41588145
    Bookkeeping work: 71.13%
     Max duplicators: 18301
      Max delimiters: 1686648
     Max total nodes: 7562654
```

</details>

### [Scott list quicksort](scott-quicksort.c)

Description: Performes a quicksort on a Scott-encoded list of 500 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):      8.777 s ±  0.058 s    [User: 8.767 s, System: 0.009 s]
  Range (min … max):    8.693 s …  8.835 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 3008506
        Commutations: 345693276
       Annihilations: 86947260
          Expansions: 629252
     Cell operations: 749500
  Barrier operations: 3753004
  Total interactions: 440780798
 Garbage collections: 3792034
  Delimiter mergings: 2877244
Delimiter extrusions: 173031257
      Total rewrites: 620481333
    Bookkeeping work: 83.94%
     Max duplicators: 3006
      Max delimiters: 289503
     Max total nodes: 1054650
```

</details>
