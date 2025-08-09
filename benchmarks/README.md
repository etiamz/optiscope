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
Annihilation interactions: 0
 Commutation interactions: 8077611
        Beta interactions: 2692537
               Expansions: 2692536
    Native function calls: 12948453
            If-then-elses: 4870845
       Total interactions: 28589446
      Garbage collections: 20390451
   Delimiter compressions: 0
     Total graph rewrites: 48979897
    Duplicators allocated: 8077611
     Delimiters allocated: 0
    Total nodes allocated: 75905268
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
Annihilation interactions: 7278199
 Commutation interactions: 37759561
        Beta interactions: 521515
               Expansions: 0
    Native function calls: 0
            If-then-elses: 0
       Total interactions: 45559275
      Garbage collections: 2067516
   Delimiter compressions: 1622053
     Total graph rewrites: 49248844
    Duplicators allocated: 3483467
     Delimiters allocated: 19660374
    Total nodes allocated: 26808608
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
Annihilation interactions: 50124995
 Commutation interactions: 400300030
        Beta interactions: 100006
               Expansions: 0
    Native function calls: 20000
            If-then-elses: 0
       Total interactions: 450545031
      Garbage collections: 10008
   Delimiter compressions: 40001
     Total graph rewrites: 450595040
    Duplicators allocated: 140001
     Delimiters allocated: 150255014
    Total nodes allocated: 150625048
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
Annihilation interactions: 84454257
 Commutation interactions: 360554484
        Beta interactions: 1007506
               Expansions: 125750
    Native function calls: 250500
            If-then-elses: 124750
       Total interactions: 446391497
      Garbage collections: 67766332
   Delimiter compressions: 2496503
     Total graph rewrites: 516654332
    Duplicators allocated: 751506
     Delimiters allocated: 236052519
    Total nodes allocated: 246589149
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
Annihilation interactions: 41800202
 Commutation interactions: 320505663
        Beta interactions: 1085106
               Expansions: 135450
    Native function calls: 180000
            If-then-elses: 89700
       Total interactions: 363660671
      Garbage collections: 28769535
   Delimiter compressions: 46806957
     Total graph rewrites: 439237163
    Duplicators allocated: 904512
     Delimiters allocated: 167379139
    Total nodes allocated: 173230541
```

</details>
