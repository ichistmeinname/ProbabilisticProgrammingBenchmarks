# Benchmark Comparisions

This repository contains benchmarks for several examples of probabilistic programs implemented in the journal version of the paper [Probabilistic Functional Logic Programming]() (to be published -- hopefully!). The paper introduces a library for probabilistic programming in the functional logic language [Curry]().

Currently, we translated the examples

[x] Rolling only sixes
[x] Grass model as Bayesian Network
[x] Testing random strings for specific characterics (being a palindrome, containing the subsequence 'bb')
[ ] more?

into 

[x] [WebPPL]()
[x] [ProbLog(2)]()
~~[ ] [Anglican]()~~

As our Curry library does not implement any inference algorithms (yet) besides calculating the whole search speach (i.e., exact inference), it does not seem reasonable to compare our examples in languages that only implement sampling-based inference algorithms (e.g., [Anglican]()).