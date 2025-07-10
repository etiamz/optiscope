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
  Time (mean ± σ):      5.660 s ±  0.027 s    [User: 5.424 s, System: 0.235 s]
  Range (min … max):    5.635 s …  5.693 s    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 14098276
Commutation interactions: 97856051
Beta interactions: 4
Native function calls: 12948453
If-then-elses: 4870845
Total interactions: 129773629
Delimiter mergings: 1346268
```

</details>

### [Fibonacci (Church numerals)](benchmarks/church-fibonacci-of-20.c)

Description: Recursively computes the 20th Fibonacci number using Church numerals & the standard Y combinator.

```
Benchmark 1: ./church-fibonacci-of-20
  Time (mean ± σ):     827.8 ms ±   2.1 ms    [User: 805.7 ms, System: 21.8 ms]
  Range (min … max):   825.8 ms … 830.4 ms    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 7604496
Commutation interactions: 40075122
Beta interactions: 515068
Native function calls: 0
If-then-elses: 0
Total interactions: 48194686
Delimiter mergings: 1844878
```

</details>

### [Church lists](benchmarks/church-list-reverse-and-sum.c)

Description: Reverses the Church-encoded list of 5000 cells & then sums all the cells up.

```
Benchmark 1: ./church-list-reverse-and-sum
  Time (mean ± σ):      1.497 s ±  0.007 s    [User: 1.444 s, System: 0.052 s]
  Range (min … max):    1.488 s …  1.504 s    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 12572493
Commutation interactions: 100150018
Beta interactions: 45004
Native function calls: 10000
If-then-elses: 0
Total interactions: 112777515
Delimiter mergings: 35014
```

</details>

### [Scott list insertion sort](benchmarks/scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):      5.102 s ±  0.015 s    [User: 4.806 s, System: 0.295 s]
  Range (min … max):    5.085 s …  5.122 s    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 46264000
Commutation interactions: 230929199
Beta interactions: 182717
Native function calls: 90300
If-then-elses: 44850
Total interactions: 277511066
Delimiter mergings: 13960660
```

</details>

### [Scott list quicksort](benchmarks/scott-quicksort.c)

Description: Performes an (inefficient) quicksort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):     14.546 s ±  0.034 s    [User: 13.658 s, System: 0.886 s]
  Range (min … max):   14.511 s … 14.583 s    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 103001358
Commutation interactions: 787926524
Beta interactions: 543927
Native function calls: 180000
If-then-elses: 89700
Total interactions: 891741509
Delimiter mergings: 123633916
```

</details>

### [Scott trees](benchmarks/scott-tree-map-and-sum.c)

Description: Multiplies by 2 all the cells in a Scott-encoded binary tree of size 2^16, then sums all the cells up.

```
Benchmark 1: ./scott-tree-map-and-sum
  Time (mean ± σ):      1.394 s ±  0.004 s    [User: 1.335 s, System: 0.058 s]
  Range (min … max):    1.389 s …  1.398 s    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 11861843
Commutation interactions: 55262990
Beta interactions: 1048579
Native function calls: 262142
If-then-elses: 0
Total interactions: 68435554
Delimiter mergings: 2031616
```

</details>
