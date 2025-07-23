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
  Time (mean ± σ):      6.002 s ±  0.017 s    [User: 5.587 s, System: 0.413 s]
  Range (min … max):    5.973 s …  6.017 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 14098275
Commutation interactions: 95481352
Beta interactions: 4
Native function calls: 12948453
If-then-elses: 4870845
Total interactions: 127398929
Garbage collections: 2178280
Delimiter mergings: 3720962
Total graph rewrites: 133298171
```

</details>

### [Fibonacci (Church numerals)](benchmarks/church-fibonacci-of-20.c)

Description: Recursively computes the 20th Fibonacci number using Church numerals & the standard Y combinator.

```
Benchmark 1: ./church-fibonacci-of-20
  Time (mean ± σ):     854.3 ms ±   2.7 ms    [User: 831.0 ms, System: 22.9 ms]
  Range (min … max):   851.2 ms … 857.5 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 7586392
Commutation interactions: 39828991
Beta interactions: 515068
Native function calls: 0
If-then-elses: 0
Total interactions: 47930451
Garbage collections: 2761986
Delimiter mergings: 1967760
Total graph rewrites: 52660197
```

</details>

### [Church lists](benchmarks/church-list-reverse-and-sum.c)

Description: Reverses the Church-encoded list of 5000 cells & then sums all the cells up.

```
Benchmark 1: ./church-list-reverse-and-sum
  Time (mean ± σ):      1.363 s ±  0.004 s    [User: 1.354 s, System: 0.008 s]
  Range (min … max):    1.357 s …  1.366 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 12557496
Commutation interactions: 87642509
Beta interactions: 45004
Native function calls: 10000
If-then-elses: 0
Total interactions: 100255009
Garbage collections: 5008
Delimiter mergings: 12522508
Total graph rewrites: 112782525
```

</details>

### [Scott list insertion sort](benchmarks/scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):      5.183 s ±  0.065 s    [User: 4.742 s, System: 0.438 s]
  Range (min … max):    5.123 s …  5.257 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 46262799
Commutation interactions: 226296785
Beta interactions: 182717
Native function calls: 90300
If-then-elses: 44850
Total interactions: 272877451
Garbage collections: 18448603
Delimiter mergings: 14226148
Total graph rewrites: 305552202
```

</details>

### [Scott list quicksort](benchmarks/scott-quicksort.c)

Description: Performes an (inefficient) quicksort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):     15.381 s ±  0.204 s    [User: 14.341 s, System: 1.034 s]
  Range (min … max):   15.191 s … 15.679 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 102911646
Commutation interactions: 786359130
Beta interactions: 543927
Native function calls: 180000
If-then-elses: 89700
Total interactions: 890084403
Garbage collections: 45629208
Delimiter mergings: 131998979
Total graph rewrites: 1067712590
```

</details>

### [Scott trees](benchmarks/scott-tree-map-and-sum.c)

Description: Multiplies by 2 all the cells in a Scott-encoded binary tree of size 2^16, then sums all the cells up.

```
Benchmark 1: ./scott-tree-map-and-sum
  Time (mean ± σ):      1.419 s ±  0.011 s    [User: 1.365 s, System: 0.054 s]
  Range (min … max):    1.408 s …  1.431 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 11599716
Commutation interactions: 52723664
Beta interactions: 1048579
Native function calls: 262142
If-then-elses: 0
Total interactions: 65634101
Garbage collections: 3735521
Delimiter mergings: 2752228
Total graph rewrites: 72121850
```

</details>
