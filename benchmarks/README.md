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
  Time (mean ± σ):      5.929 s ±  0.029 s    [User: 5.737 s, System: 0.192 s]
  Range (min … max):    5.902 s …  5.965 s    5 runs
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
  Time (mean ± σ):     857.0 ms ±  14.0 ms    [User: 830.8 ms, System: 25.9 ms]
  Range (min … max):   841.4 ms … 869.9 ms    5 runs
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
  Time (mean ± σ):      1.567 s ±  0.110 s    [User: 1.516 s, System: 0.051 s]
  Range (min … max):    1.470 s …  1.714 s    5 runs
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

Description: Performes an insertion sort on a Scott-encoded list of 150 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):      7.096 s ±  0.074 s    [User: 6.860 s, System: 0.236 s]
  Range (min … max):    7.037 s …  7.211 s    5 runs
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

Description: Performes an (inefficient) quicksort on a Scott-encoded list of 100 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):     13.637 s ±  0.166 s    [User: 13.303 s, System: 0.334 s]
  Range (min … max):   13.510 s … 13.928 s    5 runs
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
  Time (mean ± σ):      1.474 s ±  0.026 s    [User: 1.422 s, System: 0.052 s]
  Range (min … max):    1.446 s …  1.503 s    5 runs
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
