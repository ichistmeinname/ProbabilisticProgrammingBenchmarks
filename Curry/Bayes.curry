{-

You need to set the following import path in the REPL.

    :set path pflp/src/

-}

import System

import PFLP
import Float

{- Wet grass example -}

bernoulli :: Probability -> Dist Bool
bernoulli p = enum [True,False] [p, 1 - p]

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

grassWetQuery :: Probability
grassWetQuery = g_ ?? grassModel

grassWetCond :: Probability
grassWetCond = condProb [r_] [g_] grassModel

allProb :: [a -> Bool] -> Dist a -> Probability
allProb ps dx = (\x -> all (\p -> p x) ps) ?? dx

condProb :: [a -> Bool] -> [a -> Bool] -> Dist a -> Probability
condProb ps1 ps2 dx = allProb (ps1 ++ ps2) dx /. allProb ps2 dx


main :: IO ()
main = getArgs >>= \args ->
       case args of
         nArg : _ -> let n = read nArg
                     in (case n of
                          1 -> print grassWetQuery
                          2 -> print grassWetWhenRain_
                          _ -> print grassWetCond) >> return ()
         _ -> return ()
