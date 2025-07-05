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
  Time (mean ± σ):      6.000 s ±  0.033 s    [User: 5.818 s, System: 0.182 s]
  Range (min … max):    5.964 s …  6.028 s    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 17108651
Commutation interactions: 116357003
Beta interactions: 31
Native function calls: 12948453
If-then-elses: 4870845
Fixpoints: 31
Total interactions: 151285014
```

</details>

### [Fibonacci (Church numerals)](benchmarks/church-fibonacci-of-20.c)

Description: Recursively computes the 20th Fibonacci number using Church numerals & the standard Y combinator.

```
Benchmark 1: ./church-fibonacci-of-20
  Time (mean ± σ):     847.8 ms ±   4.0 ms    [User: 821.8 ms, System: 25.8 ms]
  Range (min … max):   843.5 ms … 854.3 ms    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 7633137
Commutation interactions: 40077129
Beta interactions: 521833
Native function calls: 0
If-then-elses: 0
Fixpoints: 0
Total interactions: 48232099
```

</details>

### [Church lists](benchmarks/church-list-reverse-and-sum.c)

Description: Reverses the Church-encoded list of 5000 cells & then sums all the cells up.

```
Benchmark 1: ./church-list-reverse-and-sum
  Time (mean ± σ):      1.477 s ±  0.009 s    [User: 1.427 s, System: 0.050 s]
  Range (min … max):    1.466 s …  1.487 s    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 12592493
Commutation interactions: 100155018
Beta interactions: 45004
Native function calls: 10000
If-then-elses: 0
Fixpoints: 0
Total interactions: 112802515
```

</details>

### [Scott list insertion sort](benchmarks/scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):      7.010 s ±  0.058 s    [User: 6.763 s, System: 0.247 s]
  Range (min … max):    6.948 s …  7.073 s    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 68763750
Commutation interactions: 334685485
Beta interactions: 183908
Native function calls: 90300
If-then-elses: 44850
Fixpoints: 902
Total interactions: 403769195
```

</details>

### [Scott list quicksort](benchmarks/scott-quicksort.c)

Description: Performes an (inefficient) quicksort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):     13.333 s ±  0.132 s    [User: 12.985 s, System: 0.347 s]
  Range (min … max):   13.178 s … 13.471 s    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 103271654
Commutation interactions: 788632153
Beta interactions: 545414
Native function calls: 180000
If-then-elses: 89700
Fixpoints: 1206
Total interactions: 892720127
```

</details>

### [Scott trees](benchmarks/scott-tree-map-and-sum.c)

Description: Multiplies by 2 all the cells in a Scott-encoded binary tree of size 2^16, then sums all the cells up.

```
Benchmark 1: ./scott-tree-map-and-sum
  Time (mean ± σ):      1.476 s ±  0.016 s    [User: 1.427 s, System: 0.048 s]
  Range (min … max):    1.453 s …  1.497 s    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 14221231
Commutation interactions: 64568480
Beta interactions: 1048667
Native function calls: 262142
If-then-elses: 0
Fixpoints: 66
Total interactions: 80100586
```

</details>
