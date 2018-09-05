{-

You need to set the following import path in the REPL.

    :set path pflp/src/

-}

import PFLP

data Side = One | Two | Three | Four | Five | Six deriving Eq

die :: Dist Side
die = uniform [One,Two,Three,Four,Five,Six]

allSix :: Int -> Probability
allSix n = all (==Six) ?? replicateDist n (\() -> die)

{-
ReplicateDie> allSix 5
Evaluating expression: allSix 5
1.2860082304526747e-4

real    0m0.021s
user    0m0.003s
sys     0m0.004s


ReplicateDie> allSix 10
Evaluating expression: allSix 10
1.6538171687920194e-8

real    0m0.021s
user    0m0.004s
sys     0m0.007s


ReplicateDie> allSix 20
Evaluating expression: allSix 20
2.735111227791251e-16

real    0m0.021s
user    0m0.007s
sys     0m0.008s


ReplicateDie> allSix 50
Evaluating expression: allSix 50
1.2371930760744252e-39

real    0m0.031s
user    0m0.019s
sys     0m0.009s


ReplicateDie> allSix 100
Evaluating expression: allSix 100
1.5306467074864984e-78

real    0m0.082s
user    0m0.058s
sys     0m0.015s


ReplicateDie> allSix 1000
Evaluating expression: allSix 1000
0.0

real    0m10.260s
user    0m9.062s
sys     0m1.036s
-}