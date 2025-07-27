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
  Time (mean ± σ):      5.938 s ±  0.021 s    [User: 5.575 s, System: 0.362 s]
  Range (min … max):    5.913 s …  5.961 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 14098275
 Commutation interactions: 93817275
        Beta interactions: 4
    Native function calls: 12948453
            If-then-elses: 4870845
       Total interactions: 125734852
      Garbage collections: 3524576
       Delimiter mergings: 8909616
     Total graph rewrites: 138169044
    Total nodes allocated: 130530703
        Total nodes freed: 93378056
```

</details>

### [Fibonacci (Church numerals)](benchmarks/church-fibonacci-of-20.c)

Description: Recursively computes the 20th Fibonacci number using Church numerals & the standard Y combinator.

```
Benchmark 1: ./church-fibonacci-of-20
  Time (mean ± σ):     870.0 ms ±   1.8 ms    [User: 849.8 ms, System: 19.9 ms]
  Range (min … max):   868.3 ms … 872.4 ms    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 7586392
 Commutation interactions: 38908601
        Beta interactions: 515068
    Native function calls: 0
            If-then-elses: 0
       Total interactions: 47010061
      Garbage collections: 2761986
       Delimiter mergings: 2856911
     Total graph rewrites: 52628958
    Total nodes allocated: 27977990
        Total nodes freed: 22667630
```

</details>

### [Church lists](benchmarks/church-list-reverse-and-sum.c)

Description: Reverses the Church-encoded list of 5000 cells & then sums all the cells up.

```
Benchmark 1: ./church-list-reverse-and-sum
  Time (mean ± σ):      1.410 s ±  0.003 s    [User: 1.402 s, System: 0.008 s]
  Range (min … max):    1.404 s …  1.412 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 12557496
 Commutation interactions: 87642510
        Beta interactions: 45004
    Native function calls: 10000
            If-then-elses: 0
       Total interactions: 100255010
      Garbage collections: 5008
       Delimiter mergings: 12522509
     Total graph rewrites: 112782527
    Total nodes allocated: 37787539
        Total nodes freed: 37787524
```

</details>

### [Scott list insertion sort](benchmarks/scott-insertion-sort.c)

Description: Performes an insertion sort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-insertion-sort
  Time (mean ± σ):      5.399 s ±  0.092 s    [User: 4.966 s, System: 0.430 s]
  Range (min … max):    5.296 s …  5.492 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 46262799
 Commutation interactions: 226251634
        Beta interactions: 182717
    Native function calls: 90300
            If-then-elses: 44850
       Total interactions: 272832300
      Garbage collections: 18403454
       Delimiter mergings: 14316748
     Total graph rewrites: 305552502
    Total nodes allocated: 181030126
        Total nodes freed: 130795028
```

</details>

### [Scott list quicksort](benchmarks/scott-quicksort.c)

Description: Performes an (inefficient) quicksort on a Scott-encoded list of 300 cells, then sums all the cells up.

```
Benchmark 1: ./scott-quicksort
  Time (mean ± σ):     16.164 s ±  0.112 s    [User: 15.107 s, System: 1.054 s]
  Range (min … max):   16.027 s … 16.288 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 102911646
 Commutation interactions: 785956671
        Beta interactions: 543927
    Native function calls: 180000
            If-then-elses: 89700
       Total interactions: 889681944
      Garbage collections: 45584058
       Delimiter mergings: 132402038
     Total graph rewrites: 1067668040
    Total nodes allocated: 499234732
        Total nodes freed: 404527539
```

</details>

### [Scott trees](benchmarks/scott-tree-map-and-sum.c)

Description: Multiplies by 2 all the cells in a Scott-encoded binary tree of size 2^16, then sums all the cells up.

```
Benchmark 1: ./scott-tree-map-and-sum
  Time (mean ± σ):      1.407 s ±  0.013 s    [User: 1.357 s, System: 0.049 s]
  Range (min … max):    1.392 s …  1.420 s    5 runs
```

<details>
<summary>Statistics profile</summary>

```
Annihilation interactions: 11599716
 Commutation interactions: 50397138
        Beta interactions: 1048579
    Native function calls: 262142
            If-then-elses: 0
       Total interactions: 63307575
      Garbage collections: 3735400
       Delimiter mergings: 5013355
     Total graph rewrites: 72056330
    Total nodes allocated: 39223106
        Total nodes freed: 36372114
```

</details>
