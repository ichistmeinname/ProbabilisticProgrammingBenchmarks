# Benchmark Comparisions

This repository contains benchmarks for several examples of
probabilistic programs implemented in the journal version of the paper
[Probabilistic Functional Logic Programming](https://www-ps.informatik.uni-kiel.de/%7Esad/padl2018-preprint.pdf) (to be published --
hopefully!). The paper introduces a library, [PFLP](https://github.com/finnteegen/pflp), for probabilistic programming in the functional logic language [Curry](https://www-ps.informatik.uni-kiel.de/currywiki/).

Currently, we translated the examples

[x] Rolling only sixes  
[x] Grass model as Bayesian Network  
[x] Testing random strings for specific characterics (being a
palindrome, containing the subsequence 'bb')  
[ ] more?  

into the following probabilistic languages.

[x] [WebPPL](http://webppl.org)  
[x] [ProbLog(2)](https://dtai.cs.kuleuven.be/problog/index.html)  
~~[ ] [Anglican](https://probprog.github.io/anglican/)~~  

As our Curry library does not implement any inference algorithms (yet) besides calculating the whole search speach (i.e., exact inference), it does not seem reasonable to compare our examples in languages that only implement sampling-based inference algorithms (e.g., [Anglican](https://probprog.github.io/anglican/inference/index.html)).
