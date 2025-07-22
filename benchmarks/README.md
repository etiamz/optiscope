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
  Time (mean ± σ):      5.692 s ±  0.032 s    [User: 5.526 s, System: 0.164 s]
  Range (min … max):    5.670 s …  5.746 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 14098275
Commutation interactions: 97856077
Beta interactions: 4
Native function calls: 12948453
If-then-elses: 4870845
Total interactions: 129773654
Garbage collections: 2178278
Delimiter mergings: 1346239
Total graph rewrites: 133298171
```

</details>

### [Fibonacci (Church numerals)](benchmarks/church-fibonacci-of-20.c)

Description: Recursively computes the 20th Fibonacci number using Church numerals & the standard Y combinator.

```
Benchmark 1: ./church-fibonacci-of-20
  Time (mean ± σ):     831.4 ms ±   2.8 ms    [User: 809.7 ms, System: 21.4 ms]
  Range (min … max):   827.6 ms … 835.1 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 7586392
Commutation interactions: 39982130
Beta interactions: 515068
Native function calls: 0
If-then-elses: 0
Total interactions: 48083590
Garbage collections: 2761986
Delimiter mergings: 1793785
Total graph rewrites: 52639361
```

</details>

### [Church lists](benchmarks/church-list-reverse-and-sum.c)

Description: Reverses the Church-encoded list of 5000 cells & then sums all the cells up.

```
Benchmark 1: ./church-list-reverse-and-sum
  Time (mean ± σ):      1.545 s ±  0.010 s    [User: 1.499 s, System: 0.046 s]
  Range (min … max):    1.534 s …  1.561 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 12557496
Commutation interactions: 100145014
Beta interactions: 45004
Native function calls: 10000
If-then-elses: 0
Total interactions: 112757514
Garbage collections: 5008
Delimiter mergings: 20004
Total graph rewrites: 112782526
```

</details>

### [Scott list insertion sort](benchmarks/scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):      4.893 s ±  0.034 s    [User: 4.722 s, System: 0.169 s]
  Range (min … max):    4.858 s …  4.931 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 46262799
Commutation interactions: 226477385
Beta interactions: 182717
Native function calls: 90300
If-then-elses: 44850
Total interactions: 273058051
Garbage collections: 18448303
Delimiter mergings: 14046148
Total graph rewrites: 305552502
```

</details>

### [Scott list quicksort](benchmarks/scott-quicksort.c)

Description: Performes an (inefficient) quicksort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):     14.072 s ±  0.157 s    [User: 13.720 s, System: 0.350 s]
  Range (min … max):   13.950 s … 14.329 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 102911646
Commutation interactions: 786540926
Beta interactions: 543927
Native function calls: 180000
If-then-elses: 89700
Total interactions: 890266199
Garbage collections: 45628908
Delimiter mergings: 122862734
Total graph rewrites: 1058757841
```

</details>

### [Scott trees](benchmarks/scott-tree-map-and-sum.c)

Description: Multiplies by 2 all the cells in a Scott-encoded binary tree of size 2^16, then sums all the cells up.

```
Benchmark 1: ./scott-tree-map-and-sum
  Time (mean ± σ):      1.360 s ±  0.006 s    [User: 1.302 s, System: 0.057 s]
  Range (min … max):    1.354 s …  1.367 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 11599716
Commutation interactions: 52854736
Beta interactions: 1048579
Native function calls: 262142
If-then-elses: 0
Total interactions: 65765173
Garbage collections: 4718425
Delimiter mergings: 1638252
Total graph rewrites: 72121850
```

</details>
