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
  Time (mean ± σ):      1.562 s ±  0.013 s    [User: 1.556 s, System: 0.005 s]
  Range (min … max):    1.550 s …  1.577 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
  Family reductions: 2692537
       Commutations: 8077611
      Annihilations: 0
         Expansions: 2692536
    Cell operations: 17819298
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
  Time (mean ± σ):     910.7 ms ±  12.3 ms    [User: 885.9 ms, System: 24.5 ms]
  Range (min … max):   897.4 ms … 924.4 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
  Family reductions: 521515
       Commutations: 37741839
      Annihilations: 7278199
         Expansions: 0
    Cell operations: 0
 Total interactions: 45541553
Garbage collections: 2067516
 Delimiter mergings: 1633003
     Total rewrites: 49242072
   Bookkeeping work: 82.67%
    Max duplicators: 607110
     Max delimiters: 3465471
    Max total nodes: 5741872
```

</details>

### [Church lists](church-list-reverse-and-sum.c)

Description: Reverses the Church-encoded list of 10000 cells & then sums all the cells up.

```
Benchmark 1: ./church-list-reverse-and-sum
  Time (mean ± σ):      7.359 s ±  0.079 s    [User: 6.834 s, System: 0.524 s]
  Range (min … max):    7.296 s …  7.492 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
  Family reductions: 100006
       Commutations: 400270030
      Annihilations: 50124995
         Expansions: 0
    Cell operations: 20000
 Total interactions: 450515031
Garbage collections: 10008
 Delimiter mergings: 60002
     Total rewrites: 450585041
   Bookkeeping work: 88.83%
    Max duplicators: 70002
     Max delimiters: 60006
    Max total nodes: 160030
```

</details>

### [Scott list insertion sort](scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 500 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):      8.059 s ±  0.099 s    [User: 8.051 s, System: 0.008 s]
  Range (min … max):    7.899 s …  8.161 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
  Family reductions: 1007506
       Commutations: 360804487
      Annihilations: 84454257
         Expansions: 125750
    Cell operations: 375250
 Total interactions: 446767250
Garbage collections: 67515332
 Delimiter mergings: 2497005
     Total rewrites: 516779587
   Bookkeeping work: 86.33%
    Max duplicators: 1506
     Max delimiters: 129502
    Max total nodes: 240103
```

</details>

### [Scott list quicksort](scott-quicksort.c)

Description: Performes an (inefficient) quicksort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):      5.983 s ±  0.016 s    [User: 5.977 s, System: 0.005 s]
  Range (min … max):    5.959 s …  5.999 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
  Family reductions: 1085106
       Commutations: 320234168
      Annihilations: 41755949
         Expansions: 135450
    Cell operations: 269700
 Total interactions: 363480373
Garbage collections: 28769535
 Delimiter mergings: 46807259
     Total rewrites: 439057167
   Bookkeeping work: 90.67%
    Max duplicators: 3015
     Max delimiters: 150603
    Max total nodes: 440103
```

</details>
