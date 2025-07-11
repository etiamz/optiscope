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
  Time (mean ± σ):      5.615 s ±  0.021 s    [User: 5.434 s, System: 0.179 s]
  Range (min … max):    5.591 s …  5.646 s    5 runs
```

<details>
<summary>Statistics profile</summary>

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
  Time (mean ± σ):     821.8 ms ±   2.7 ms    [User: 798.0 ms, System: 23.5 ms]
  Range (min … max):   819.0 ms … 825.4 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

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
  Time (mean ± σ):      1.490 s ±  0.009 s    [User: 1.440 s, System: 0.050 s]
  Range (min … max):    1.479 s …  1.502 s    5 runs
```

<details>
<summary>Statistics profile</summary>

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
  Time (mean ± σ):      4.960 s ±  0.011 s    [User: 4.782 s, System: 0.178 s]
  Range (min … max):    4.944 s …  4.975 s    5 runs
```

<details>
<summary>Statistics profile</summary>

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
  Time (mean ± σ):     13.601 s ±  0.045 s    [User: 13.265 s, System: 0.334 s]
  Range (min … max):   13.552 s … 13.651 s    5 runs
```

<details>
<summary>Statistics profile</summary>

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
  Time (mean ± σ):      1.394 s ±  0.009 s    [User: 1.344 s, System: 0.050 s]
  Range (min … max):    1.387 s …  1.409 s    5 runs
```

<details>
<summary>Statistics profile</summary>

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
