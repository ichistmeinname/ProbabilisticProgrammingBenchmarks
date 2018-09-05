{-

You need to set the following import path in the REPL.

    :set path pflp/src/

-}

import PFLP
import Float

{- Wet grass example -}

rain :: Dist Bool
rain = bernoulli 0.2

sprinkler :: Bool -> Dist Bool
sprinkler False = bernoulli 0.4 
sprinkler True = bernoulli 0.01

grassWet :: Bool -> Bool -> Dist Bool
grassWet False False = bernoulli 0.0
grassWet False True  = bernoulli 0.8
grassWet True  False = bernoulli 0.9
grassWet True  True  = bernoulli 0.99

data GrassModel = M { r_ :: Bool, s_ :: Bool, g_ :: Bool}

grassModel :: Dist GrassModel
grassModel = rain >>>= \r -> sprinkler r >>>= \s -> grassWet s r >>>= \g -> certainly (M r s g)

grassWetWhenRain_ :: Probability
grassWetWhenRain_ = (\model -> r_ model && g_ model) ?? grassModel