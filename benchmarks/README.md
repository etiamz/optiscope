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

Description: Recursively computes the 30th Fibonacci number using native cells & the built-in fixpoint operator.

```
Benchmark 1: ./fibonacci-of-30
  Time (mean ± σ):      1.493 s ±  0.102 s    [User: 1.486 s, System: 0.006 s]
  Range (min … max):    1.430 s …  1.674 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 0
 Commutation interactions: 8077611
        Beta interactions: 2692537
               Expansions: 2692536
    Native function calls: 12948453
            If-then-elses: 4870845
       Total interactions: 28589446
      Garbage collections: 20390451
       Delimiter mergings: 0
     Total graph rewrites: 48979897
    Duplicators allocated: 8077611
     Delimiters allocated: 0
    Total nodes allocated: 75905268
```

</details>

### [Fibonacci (Church numerals)](church-y-fibonacci-of-20.c)

Description: Recursively computes the 20th Fibonacci number using Church numerals & the standard Y combinator.

```
Benchmark 1: ./church-y-fibonacci-of-20
  Time (mean ± σ):     883.0 ms ±   3.5 ms    [User: 860.9 ms, System: 21.5 ms]
  Range (min … max):   879.1 ms … 887.8 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 7586395
 Commutation interactions: 40055407
        Beta interactions: 515068
               Expansions: 0
    Native function calls: 0
            If-then-elses: 0
       Total interactions: 48156870
      Garbage collections: 2761986
       Delimiter mergings: 1710153
     Total graph rewrites: 52629009
    Duplicators allocated: 3395304
     Delimiters allocated: 20924457
    Total nodes allocated: 27978004
```

</details>

### [Church lists](church-list-reverse-and-sum.c)

Description: Reverses the Church-encoded list of 10000 cells & then sums all the cells up.

```
Benchmark 1: ./church-list-reverse-and-sum
  Time (mean ± σ):      6.144 s ±  0.072 s    [User: 6.133 s, System: 0.011 s]
  Range (min … max):    6.093 s …  6.269 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 50124995
 Commutation interactions: 400290031
        Beta interactions: 90004
               Expansions: 0
    Native function calls: 20000
            If-then-elses: 0
       Total interactions: 450525030
      Garbage collections: 10008
       Delimiter mergings: 40000
     Total graph rewrites: 450575038
    Duplicators allocated: 140001
     Delimiters allocated: 150245014
    Total nodes allocated: 150595044
```

</details>

### [Scott list insertion sort](scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 500 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):      7.243 s ±  0.067 s    [User: 7.236 s, System: 0.007 s]
  Range (min … max):    7.182 s …  7.353 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 84454257
 Commutation interactions: 360428235
        Beta interactions: 881254
               Expansions: 125750
    Native function calls: 250500
            If-then-elses: 124750
       Total interactions: 446138996
      Garbage collections: 66389568
       Delimiter mergings: 2371753
     Total graph rewrites: 514900317
    Duplicators allocated: 751506
     Delimiters allocated: 235551015
    Total nodes allocated: 243833121
```

</details>

### [Scott list quicksort](scott-quicksort.c)

Description: Performes an (inefficient) quicksort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):      5.614 s ±  0.065 s    [User: 5.606 s, System: 0.007 s]
  Range (min … max):    5.564 s …  5.726 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 41800200
 Commutation interactions: 320414188
        Beta interactions: 994504
               Expansions: 135450
    Native function calls: 180000
            If-then-elses: 89700
       Total interactions: 363478592
      Garbage collections: 28627773
       Delimiter mergings: 46717266
     Total graph rewrites: 438823631
    Duplicators allocated: 904512
     Delimiters allocated: 167241000
    Total nodes allocated: 172722178
```

</details>

### [Scott trees](scott-tree-map-and-sum.c)

Description: Multiplies by 2 all the cells in a Scott-encoded binary tree of size 2^16, then sums all the cells up.

```
Benchmark 1: ./scott-tree-map-and-sum
  Time (mean ± σ):      1.017 s ±  0.006 s    [User: 0.971 s, System: 0.045 s]
  Range (min … max):    1.008 s …  1.025 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 9174902
 Commutation interactions: 40468491
        Beta interactions: 1507314
               Expansions: 262140
    Native function calls: 262142
            If-then-elses: 0
       Total interactions: 51412849
      Garbage collections: 4587512
       Delimiter mergings: 1671022
     Total graph rewrites: 57671383
    Duplicators allocated: 393212
     Delimiters allocated: 25263975
    Total nodes allocated: 32669493
```

</details>
