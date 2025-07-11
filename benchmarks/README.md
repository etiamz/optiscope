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
  Time (mean ± σ):      5.663 s ±  0.012 s    [User: 5.484 s, System: 0.177 s]
  Range (min … max):    5.649 s …  5.681 s    5 runs
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
Delimiter mergings: 1346239
```

</details>

### [Fibonacci (Church numerals)](benchmarks/church-fibonacci-of-20.c)

Description: Recursively computes the 20th Fibonacci number using Church numerals & the standard Y combinator.

```
Benchmark 1: ./church-fibonacci-of-20
  Time (mean ± σ):     823.3 ms ±   2.9 ms    [User: 800.2 ms, System: 22.7 ms]
  Range (min … max):   820.8 ms … 827.9 ms    5 runs
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
Delimiter mergings: 1844362
```

</details>

### [Church lists](benchmarks/church-list-reverse-and-sum.c)

Description: Reverses the Church-encoded list of 5000 cells & then sums all the cells up.

```
Benchmark 1: ./church-list-reverse-and-sum
  Time (mean ± σ):      1.535 s ±  0.006 s    [User: 1.486 s, System: 0.048 s]
  Range (min … max):    1.526 s …  1.541 s    5 runs
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
Delimiter mergings: 30009
```

</details>

### [Scott list insertion sort](benchmarks/scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):      4.975 s ±  0.030 s    [User: 4.796 s, System: 0.178 s]
  Range (min … max):    4.930 s …  4.999 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 46264000
Commutation interactions: 230933402
Beta interactions: 182717
Native function calls: 90300
If-then-elses: 44850
Total interactions: 277515269
Delimiter mergings: 13956461
```

</details>

### [Scott list quicksort](benchmarks/scott-quicksort.c)

Description: Performes an (inefficient) quicksort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):     13.697 s ±  0.017 s    [User: 13.374 s, System: 0.322 s]
  Range (min … max):   13.680 s … 13.715 s    5 runs
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
Delimiter mergings: 123359412
```

</details>

### [Scott trees](benchmarks/scott-tree-map-and-sum.c)

Description: Multiplies by 2 all the cells in a Scott-encoded binary tree of size 2^16, then sums all the cells up.

```
Benchmark 1: ./scott-tree-map-and-sum
  Time (mean ± σ):      1.409 s ±  0.008 s    [User: 1.360 s, System: 0.050 s]
  Range (min … max):    1.401 s …  1.416 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 11861843
Commutation interactions: 55263143
Beta interactions: 1048579
Native function calls: 262142
If-then-elses: 0
Total interactions: 68435707
Delimiter mergings: 2031467
```

</details>
