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
  Time (mean ± σ):      5.950 s ±  0.035 s    [User: 5.773 s, System: 0.176 s]
  Range (min … max):    5.904 s …  5.985 s    5 runs
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
  Time (mean ± σ):     24.989 s ±  0.125 s    [User: 24.948 s, System: 0.040 s]
  Range (min … max):   24.853 s … 25.160 s    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 20464077
Commutation interactions: 2238539577
Beta interactions: 521833
Native function calls: 0
If-then-elses: 0
Fixpoints: 0
Total interactions: 2259525487
```

</details>

### [Church lists](benchmarks/church-list-reverse-and-sum.c)

Description: Reverses the Church-encoded list of 5000 cells & then sums all the cells up.

```
Benchmark 1: ./church-list-reverse-and-sum
  Time (mean ± σ):      4.675 s ±  0.046 s    [User: 4.611 s, System: 0.064 s]
  Range (min … max):    4.631 s …  4.750 s    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 25179997
Commutation interactions: 275405052
Beta interactions: 45004
Native function calls: 10000
If-then-elses: 0
Fixpoints: 0
Total interactions: 300640053
```

</details>

### [Scott list insertion sort](benchmarks/scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 150 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):      6.569 s ±  0.022 s    [User: 6.457 s, System: 0.111 s]
  Range (min … max):    6.536 s …  6.596 s    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 64821613
Commutation interactions: 370771136
Beta interactions: 46958
Native function calls: 22650
If-then-elses: 11175
Fixpoints: 452
Total interactions: 435673984
```

</details>

### [Scott list quicksort](benchmarks/scott-quicksort.c)

Description: Performes an (inefficient) quicksort on a Scott-encoded list of 100 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):     27.112 s ±  0.287 s    [User: 27.064 s, System: 0.047 s]
  Range (min … max):   26.866 s … 27.584 s    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 104534500
Commutation interactions: 1062496348
Beta interactions: 61814
Native function calls: 20000
If-then-elses: 9900
Fixpoints: 406
Total interactions: 1167122968
```

</details>

### [Scott trees](benchmarks/scott-tree-map-and-sum.c)

Description: Multiplies by 2 all the cells in a Scott-encoded binary tree of size 2^16, then sums all the cells up.

```
Benchmark 1: ./scott-tree-map-and-sum
  Time (mean ± σ):      3.855 s ±  0.061 s    [User: 3.794 s, System: 0.061 s]
  Range (min … max):    3.806 s …  3.942 s    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 40861148
Commutation interactions: 234219681
Beta interactions: 1048667
Native function calls: 262142
If-then-elses: 0
Fixpoints: 66
Total interactions: 276391704
```

</details>

### [Owl explosion](benchmarks/owl-explosion.c)

Description: Evaluates an application sequence of 5000 + 1 Owl combinators `(λa. λb. (b (a b)))`.

```
Benchmark 1: ./owl-explosion
  Time (mean ± σ):      3.331 s ±  0.043 s    [User: 3.136 s, System: 0.194 s]
  Range (min … max):    3.275 s …  3.371 s    5 runs
```

<details>
<summary>Interactions count</summary>

```
Annihilation interactions: 24985002
Commutation interactions: 124945006
Beta interactions: 9998
Native function calls: 0
If-then-elses: 0
Fixpoints: 0
Total interactions: 149940006
```

</details>
