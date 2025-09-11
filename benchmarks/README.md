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
  Time (mean ± σ):     900.1 ms ±   1.4 ms    [User: 836.3 ms, System: 63.4 ms]
  Range (min … max):   898.5 ms … 902.1 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 158859
        Commutations: 34235501
       Annihilations: 3664080
          Expansions: 11624
     Cell operations: 68248
  Barrier operations: 48585
  Total interactions: 38186897
 Garbage collections: 480329
  Delimiter mergings: 14643540
Delimiter extrusions: 381912
      Total rewrites: 53692678
    Bookkeeping work: 60.34%
     Max duplicators: 67052
      Max delimiters: 1720506
     Max total nodes: 18573912
```

</details>

### [Scott list insertion sort](scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):      1.604 s ±  0.012 s    [User: 1.597 s, System: 0.007 s]
  Range (min … max):    1.591 s …  1.622 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 4015006
        Commutations: 16515507
       Annihilations: 1498500
          Expansions: 501500
     Cell operations: 1500500
  Barrier operations: 4505503
  Total interactions: 28536516
 Garbage collections: 21529583
  Delimiter mergings: 4500501
Delimiter extrusions: 4998999
      Total rewrites: 59565599
    Bookkeeping work: 38.63%
     Max duplicators: 3006
      Max delimiters: 12022
     Max total nodes: 502604
```

</details>

### [Scott list merge sort](scott-merge-sort.c)

Description: Performes a merge sort on a Scott-encoded list of 1000 cells, then sums all the cells up.

```
Benchmark 1: ./scott-merge-sort
  Time (mean ± σ):      1.268 s ±  0.002 s    [User: 1.240 s, System: 0.028 s]
  Range (min … max):    1.265 s …  1.270 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 223404
        Commutations: 49821002
       Annihilations: 5521639
          Expansions: 16907
     Cell operations: 28931
  Barrier operations: 228589
  Total interactions: 55840472
 Garbage collections: 814208
  Delimiter mergings: 11131214
Delimiter extrusions: 9893731
      Total rewrites: 77679625
    Bookkeeping work: 87.53%
     Max duplicators: 18301
      Max delimiters: 1688754
     Max total nodes: 7565427
```

</details>

### [Scott list quicksort](scott-quicksort.c)

Description: Performes a quicksort on a Scott-encoded list of 500 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):      8.876 s ±  0.017 s    [User: 8.867 s, System: 0.009 s]
  Range (min … max):    8.852 s …  8.891 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
   Family reductions: 3008506
        Commutations: 348433313
       Annihilations: 87567521
          Expansions: 375750
     Cell operations: 749500
  Barrier operations: 2632743
  Total interactions: 442767333
 Garbage collections: 5350134
  Delimiter mergings: 3623249
Delimiter extrusions: 173031257
      Total rewrites: 624771973
    Bookkeeping work: 84.10%
     Max duplicators: 5015
      Max delimiters: 292975
     Max total nodes: 1071753
```

</details>
