# Benchmark Comparisions

This repository contains benchmarks for several examples of
probabilistic programs implemented with [PFLP](https://github.com/finnteegen/pflp), a library for probabilistic programming in the functional logic language [Curry](https://www-ps.informatik.uni-kiel.de/currywiki/).

Currently, we translated the examples

[x] Rolling only sixes ([dice example](https://rawgit.com/ichistmeinname/ProbabilisticProgrammingBenchmarks/master/html/ReplicateDie.html)  
[x] Grass model as Bayesian Network ([grass example](https://rawgit.com/ichistmeinname/ProbabilisticProgrammingBenchmarks/master/html/Bayes.html))  
[x] Testing random strings for specific characterics (being a
palindrome, containing the subsequence 'bb') ([naive bb](https://rawgit.com/ichistmeinname/ProbabilisticProgrammingBenchmarks/master/html/StringsBs.html), [naive palindrome](https://rawgit.com/ichistmeinname/ProbabilisticProgrammingBenchmarks/master/html/StringsPalindrome.html), [fast palindrome](https://rawgit.com/ichistmeinname/ProbabilisticProgrammingBenchmarks/master/html/StringsPalindromeFast.html))  
[ ] more?  

into the following probabilistic languages.

[x] [WebPPL](http://webppl.org)  
[x] [ProbLog(2)](https://dtai.cs.kuleuven.be/problog/index.html)  
~~[ ] [Anglican](https://probprog.github.io/anglican/)~~  

As our Curry library does not implement any inference algorithms (yet) besides calculating the whole search speach (i.e., exact inference), it does not seem reasonable to compare our examples in languages that only implement sampling-based inference algorithms (e.g., [Anglican](https://probprog.github.io/anglican/inference/index.html)).

# Install Dependencies

## Shake
In order to use `build.sh` you need a local shake binary. If you have
a global installation of shake, you can just modify the `build.sh`
accordingly.
Otherwise you should install shake as follows.

```
cabal sandbox init
cabal install shake
```

## Python + pip
As we need to use ProbLog as library you need a working `python`
installation as well as its package manager `pip` installed.

## npm
In order to benchmark WebPPL programs an `npm` installation is needed.

## kics2

Last but not least as we're benchmarking a Curry library, so a Curry
installation is necessary. We recommend to use [kics2](https://www-ps.informatik.uni-kiel.de/kics2/download.html).

## pflp

The Curry library we want to benchmark, PFLP, is included as a submodule.

```
submodule init
submodule update
```

# Benchmarks

If all the dependencies are in scope, you can execute the build-script
as it should install ProbLog, WebPPL, the necessary Curry
executables, the Haskell package `bench` to run benchmarks on the
command line, and actually executing the benchmarks.

The output is then located in `html/` and waits to be opened in a browser.

```
ichistmeinname$ ./build.sh
[1 of 1] Compiling Main             ( Build.hs, _shake/Main.o )
Linking _shake/build ...
# mkdir (for benchmarks)
# command (for bench)
.cabal-sandbox/bin/bench
# command (for ProbLog)
/Users/sad/Library/Python/2.7/bin/problog
# command (for WebPPL)
# command (for WebPPL)
WebPPL/node_modules/.bin/webppl
# command (for Curry)
/Users/sad/Documents/Programming/kics2/bin/kics2
Save Curry executable ./Bayes
# kics2 (for Curry/Bayes)
readlink: illegal option -- f
usage: readlink [-n] [file ...]
Changing working directory to Curry/
Benchmark bayesian network
# bench (for bayes)
benchmarking bench/./Curry/Bayes  ""
time                 25.06 ms   (24.83 ms .. 25.34 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 24.87 ms   (24.69 ms .. 25.04 ms)
std dev              378.0 μs   (273.3 μs .. 547.2 μs)

benchmarking bench/python ProbLog/bayes.py  ""
time                 128.5 ms   (128.0 ms .. 129.4 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 128.5 ms   (128.0 ms .. 129.5 ms)
std dev              1.080 ms   (133.3 μs .. 1.700 ms)
variance introduced by outliers: 11% (moderately inflated)

benchmarking bench/./WebPPL/node_modules/.bin/webppl WebPPL/bayes.wppl  ""
time                 1.524 s    (1.388 s .. 1.595 s)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 1.560 s    (1.529 s .. 1.571 s)
std dev              21.01 ms   (3.969 ms .. 27.15 ms)
variance introduced by outliers: 19% (moderately inflated)

[...]
```
