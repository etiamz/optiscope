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

Description: Performes a bubble sort on a Scott-encoded list of 150 cells, then sums all the cells up.

```
Benchmark 1: ./scott-bubble-sort
  Time (mean ± σ):     799.3 ms ±   3.2 ms    [User: 740.5 ms, System: 58.4 ms]
  Range (min … max):   796.7 ms … 804.9 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 158859
        Commutations: 34235502
       Annihilations: 3664080
          Expansions: 34127
     Cell operations: 68248
  Barrier operations: 48585
  Total interactions: 38209401
 Garbage collections: 398900
  Delimiter mergings: 14643540
Delimiter extrusions: 381912
      Total rewrites: 53633753
    Bookkeeping work: 60.30%
     Max duplicators: 67052
      Max delimiters: 1720506
     Max total nodes: 18573912
```

</details>

### [Scott list insertion sort](scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):     835.8 ms ±   8.8 ms    [User: 828.2 ms, System: 7.2 ms]
  Range (min … max):   826.7 ms … 850.2 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 4015006
        Commutations: 16516507
       Annihilations: 1498500
          Expansions: 1003003
     Cell operations: 1500500
  Barrier operations: 4505502
  Total interactions: 29039018
 Garbage collections: 8019015
  Delimiter mergings: 4500500
Delimiter extrusions: 4998999
      Total rewrites: 46557532
    Bookkeeping work: 48.35%
     Max duplicators: 3003
      Max delimiters: 4009
     Max total nodes: 500989
```

</details>

### [Scott list merge sort](scott-merge-sort.c)

Description: Performes a merge sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-merge-sort
  Time (mean ± σ):      1.138 s ±  0.003 s    [User: 1.110 s, System: 0.027 s]
  Range (min … max):    1.134 s …  1.142 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 223404
        Commutations: 49820339
       Annihilations: 5519753
          Expansions: 40746
     Cell operations: 28931
  Barrier operations: 228589
  Total interactions: 55861762
 Garbage collections: 468952
  Delimiter mergings: 11127218
Delimiter extrusions: 9893731
      Total rewrites: 77351663
    Bookkeeping work: 85.90%
     Max duplicators: 18301
      Max delimiters: 1686664
     Max total nodes: 7561655
```

</details>

### [Scott list quicksort](scott-quicksort.c)

Description: Performes a quicksort on a Scott-encoded list of 500 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):      7.943 s ±  0.052 s    [User: 7.932 s, System: 0.009 s]
  Range (min … max):    7.905 s …  8.029 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 3008506
        Commutations: 348557066
       Annihilations: 87567022
          Expansions: 629252
     Cell operations: 749500
  Barrier operations: 2632743
  Total interactions: 443144089
 Garbage collections: 3792034
  Delimiter mergings: 3623249
Delimiter extrusions: 173031257
      Total rewrites: 623590629
    Bookkeeping work: 84.08%
     Max duplicators: 3006
      Max delimiters: 289503
     Max total nodes: 1054486
```

</details>
