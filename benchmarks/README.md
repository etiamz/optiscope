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
  Time (mean ± σ):      5.897 s ±  0.031 s    [User: 5.716 s, System: 0.180 s]
  Range (min … max):    5.864 s …  5.939 s    5 runs
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
  Time (mean ± σ):     830.6 ms ±   3.4 ms    [User: 809.4 ms, System: 21.0 ms]
  Range (min … max):   825.8 ms … 833.8 ms    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 7725926
Commutation interactions: 39696913
Beta interactions: 521833
Native function calls: 0
If-then-elses: 0
Fixpoints: 0
Total interactions: 47944672
```

</details>

### [Church lists](benchmarks/church-list-reverse-and-sum.c)

Description: Reverses the Church-encoded list of 5000 cells & then sums all the cells up.

```
Benchmark 1: ./church-list-reverse-and-sum
  Time (mean ± σ):      1.441 s ±  0.002 s    [User: 1.390 s, System: 0.051 s]
  Range (min … max):    1.439 s …  1.445 s    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 12642491
Commutation interactions: 100280016
Beta interactions: 45004
Native function calls: 10000
If-then-elses: 0
Fixpoints: 0
Total interactions: 112977511
```

</details>

### [Scott list insertion sort](benchmarks/scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):      6.954 s ±  0.024 s    [User: 6.716 s, System: 0.237 s]
  Range (min … max):    6.934 s …  6.995 s    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 68763750
Commutation interactions: 334503089
Beta interactions: 183908
Native function calls: 90300
If-then-elses: 44850
Fixpoints: 902
Total interactions: 403586799
```

</details>

### [Scott list quicksort](benchmarks/scott-quicksort.c)

Description: Performes an (inefficient) quicksort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):     13.254 s ±  0.102 s    [User: 12.909 s, System: 0.344 s]
  Range (min … max):   13.121 s … 13.367 s    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 103271654
Commutation interactions: 788087056
Beta interactions: 545414
Native function calls: 180000
If-then-elses: 89700
Fixpoints: 1206
Total interactions: 892175030
```

</details>

### [Scott trees](benchmarks/scott-tree-map-and-sum.c)

Description: Multiplies by 2 all the cells in a Scott-encoded binary tree of size 2^16, then sums all the cells up.

```
Benchmark 1: ./scott-tree-map-and-sum
  Time (mean ± σ):      1.449 s ±  0.018 s    [User: 1.397 s, System: 0.051 s]
  Range (min … max):    1.435 s …  1.480 s    5 runs
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
