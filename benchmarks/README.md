# Benchmarks

<details>
<summary>System information</summary>

```
                          ./+o+-       etiamz@etiamz
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

### [Fibonacci (native cells)](fibonacci-of-30.c)

Description: Exponentially computes the 30th Fibonacci number using native cells & the built-in fixpoint operator.

```
Benchmark 1: ./fibonacci-of-30
  Time (mean ± σ):      1.396 s ±  0.019 s    [User: 1.389 s, System: 0.006 s]
  Range (min … max):    1.380 s …  1.428 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
  Family reductions: 2692537
       Commutations: 8077611
      Annihilations: 0
         Expansions: 2692536
    Cell operations: 17819298
 Barrier operations: 0
 Total interactions: 31281982
Garbage collections: 20390451
 Delimiter mergings: 0
     Total rewrites: 51672433
   Bookkeeping work: 0.00%
    Max duplicators: 3
     Max delimiters: 0
    Max total nodes: 198
```

</details>

### [Fibonacci (Church numerals)](church-fix-fibonacci-of-20.c)

Description: Exponentially computes the 20th Fibonacci number using Church numerals & the standard Y combinator.

```
Benchmark 1: ./church-fix-fibonacci-of-20
  Time (mean ± σ):     850.4 ms ±   3.6 ms    [User: 827.2 ms, System: 22.3 ms]
  Range (min … max):   846.7 ms … 856.3 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
  Family reductions: 521515
       Commutations: 36229956
      Annihilations: 7048197
         Expansions: 0
    Cell operations: 0
 Barrier operations: 599808
 Total interactions: 44399476
Garbage collections: 2067516
 Delimiter mergings: 624537
     Total rewrites: 47091529
   Bookkeeping work: 81.32%
    Max duplicators: 607110
     Max delimiters: 3465467
    Max total nodes: 5741869
```

</details>

### [Church lists](church-list-reverse-and-sum.c)

Description: Reverses the Church-encoded list of 10000 cells & then sums all the cells up.

```
Benchmark 1: ./church-list-reverse-and-sum
  Time (mean ± σ):      5.800 s ±  0.014 s    [User: 5.787 s, System: 0.012 s]
  Range (min … max):    5.786 s …  5.818 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
  Family reductions: 100006
       Commutations: 400290015
      Annihilations: 50124995
         Expansions: 0
    Cell operations: 20000
 Barrier operations: 20005
 Total interactions: 450555021
Garbage collections: 10008
 Delimiter mergings: 59998
     Total rewrites: 450625027
   Bookkeeping work: 88.82%
    Max duplicators: 70002
     Max delimiters: 60006
    Max total nodes: 160030
```

</details>

### [Scott list insertion sort](scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 500 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):      7.679 s ±  0.044 s    [User: 7.669 s, System: 0.007 s]
  Range (min … max):    7.637 s …  7.749 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
  Family reductions: 1007506
       Commutations: 358796995
      Annihilations: 84454257
         Expansions: 125750
    Cell operations: 375250
 Barrier operations: 256492
 Total interactions: 445016250
Garbage collections: 67515332
 Delimiter mergings: 500501
     Total rewrites: 513032083
   Bookkeeping work: 86.18%
    Max duplicators: 1506
     Max delimiters: 129502
    Max total nodes: 239812
```

</details>

### [Scott list quicksort](scott-quicksort.c)

Description: Performes an (inefficient) quicksort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):      3.705 s ±  0.012 s    [User: 3.697 s, System: 0.007 s]
  Range (min … max):    3.690 s …  3.718 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
  Family reductions: 1085106
       Commutations: 183825857
      Annihilations: 41622888
         Expansions: 135450
    Cell operations: 269700
 Barrier operations: 721823
 Total interactions: 227660824
Garbage collections: 28769535
 Delimiter mergings: 806709
     Total rewrites: 257237068
   Bookkeeping work: 84.06%
    Max duplicators: 3015
     Max delimiters: 150603
    Max total nodes: 439804
```

</details>
