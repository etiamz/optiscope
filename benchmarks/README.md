# Benchmarks

<details>
<summary>System information</summary>

```
                          ./+o+-       etiams@etiams
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

### [Fibonacci (native cells)](benchmarks/fibonacci-of-30.c)

Description: Recursively computes the 30th Fibonacci number using native cells & the built-in fixpoint operator.

```
Benchmark 1: ./fibonacci-of-30
  Time (mean ± σ):      5.602 s ±  0.028 s    [User: 5.439 s, System: 0.163 s]
  Range (min … max):    5.568 s …  5.631 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 14098276
Commutation interactions: 97856082
Beta interactions: 4
Native function calls: 12948453
If-then-elses: 4870845
Total interactions: 129773660
Garbage collections: 2178278
Delimiter mergings: 1346239
Total graph rewrites: 133298177
```

</details>

### [Fibonacci (Church numerals)](benchmarks/church-fibonacci-of-20.c)

Description: Recursively computes the 20th Fibonacci number using Church numerals & the standard Y combinator.

```
Benchmark 1: ./church-fibonacci-of-20
  Time (mean ± σ):     825.1 ms ±   2.0 ms    [User: 801.8 ms, System: 23.1 ms]
  Range (min … max):   822.7 ms … 827.9 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 7604496
Commutation interactions: 40075640
Beta interactions: 515068
Native function calls: 0
If-then-elses: 0
Total interactions: 48195204
Garbage collections: 2761986
Delimiter mergings: 1844362
Total graph rewrites: 52801552
```

</details>

### [Church lists](benchmarks/church-list-reverse-and-sum.c)

Description: Reverses the Church-encoded list of 5000 cells & then sums all the cells up.

```
Benchmark 1: ./church-list-reverse-and-sum
  Time (mean ± σ):      1.519 s ±  0.007 s    [User: 1.470 s, System: 0.048 s]
  Range (min … max):    1.511 s …  1.531 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 12572493
Commutation interactions: 100160025
Beta interactions: 45004
Native function calls: 10000
If-then-elses: 0
Total interactions: 112787522
Garbage collections: 5008
Delimiter mergings: 30009
Total graph rewrites: 112822539
```

</details>

### [Scott list insertion sort](benchmarks/scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):      4.821 s ±  0.017 s    [User: 4.672 s, System: 0.149 s]
  Range (min … max):    4.792 s …  4.838 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 46264000
Commutation interactions: 226433751
Beta interactions: 182717
Native function calls: 90300
If-then-elses: 44850
Total interactions: 273015618
Garbage collections: 18448605
Delimiter mergings: 13956461
Total graph rewrites: 305420684
```

</details>

### [Scott list quicksort](benchmarks/scott-quicksort.c)

Description: Performes an (inefficient) quicksort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):     14.093 s ±  0.172 s    [User: 13.423 s, System: 0.667 s]
  Range (min … max):   13.938 s … 14.383 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 103001358
Commutation interactions: 788201032
Beta interactions: 543927
Native function calls: 180000
If-then-elses: 89700
Total interactions: 892016017
Garbage collections: 45629806
Delimiter mergings: 123359412
Total graph rewrites: 1061005235
```

</details>

### [Scott trees](benchmarks/scott-tree-map-and-sum.c)

Description: Multiplies by 2 all the cells in a Scott-encoded binary tree of size 2^16, then sums all the cells up.

```
Benchmark 1: ./scott-tree-map-and-sum
  Time (mean ± σ):      1.408 s ±  0.002 s    [User: 1.354 s, System: 0.054 s]
  Range (min … max):    1.406 s …  1.411 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 11796324
Commutation interactions: 55066586
Beta interactions: 1048579
Native function calls: 262142
If-then-elses: 0
Total interactions: 68173631
Garbage collections: 4783961
Delimiter mergings: 2031467
Total graph rewrites: 74989059
```

</details>
