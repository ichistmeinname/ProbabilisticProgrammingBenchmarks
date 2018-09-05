{-

You need to set the following import path in the REPL.

    :set path pflp/src/

-}

import PFLP

pickChar :: Dist Char
pickChar = uniform ['a','b']

randomString :: Int -> Dist String
randomString n = replicateDist n (\ () -> pickChar)

reverse :: [a] -> [a]
reverse xs = rev xs []
 where rev ys acc = case ys of
                         [] -> acc
                         (z:zs) -> rev zs (z:acc)

palindrome :: String -> Bool
palindrome str = str == reverse str

consecutiveBs :: String -> Bool
consecutiveBs bs = case  bs of
                 []                  -> False
                 ('b'  : 'b' : _  )  -> True
                 (_    : bs       )  -> consecutiveBs bs

{-
tplp> palindrome ?? randomString 30
Evaluating expression: palindrome ?? randomString 30
3.0517578125e-5

real    0m15.307s
user    0m13.788s
sys     0m1.005s


tplp> palindrome ?? randomString 25
Evaluating expression: palindrome ?? randomString 25
2.44140625e-4

real    0m2.889s
user    0m2.687s
sys     0m0.190s


tplp> palindrome ?? randomString 20
Evaluating expression: palindrome ?? randomString 20
9.765625e-4

real    0m0.355s
user    0m0.293s
sys     0m0.031s


tplp> palindrome ?? randomString 10
Evaluating expression: palindrome ?? randomString 10
3.125e-2

real    0m0.020s
user    0m0.005s
sys     0m0.005s


tplp> palindrome ?? randomString 35
Evaluating expression: palindrome ?? randomString 35
7.62939453125e-6

real    4m58.340s
user    2m38.904s
sys     1m39.442s
-}

palindromeEfficient :: Int -> Dist (Bool, String)
palindromeEfficient n = palindrome' 1 n
 where palindrome' n1 n2  | n1 == n2   = pickChar >>>= \ c -> certainly (True,[c])
                          | n1 > n2    = certainly (True, [])
                          | otherwise  = pickChar >>>= \c1 ->
                                         pickChar >>>= \c2 ->
                                         palindrome' (n1+1) (n2-1) >>>= \ (b,cs) ->
                                         certainly (c1 == c2 && b, c1 : cs ++ [c2])

-- As weird as it sounds, but this version is less efficient...
-- Evaluating expression: id ?? palindromeEfficient2 30
-- 3.0517578125e-5

-- real	0m24.715s
-- user	0m22.332s
-- sys	0m2.300s
palindromeEfficient2 :: Int -> Dist Bool
palindromeEfficient2 n = palindrome' 1 n
 where palindrome' n1 n2  | n1 == n2   = pickChar >>>= \ _ -> certainly True
                          | n1 > n2    = certainly True
                          | otherwise  = pickChar >>>= \c1 ->
                                         pickChar >>>= \c2 ->
                                         if c1 == c2 then palindrome' (n1+1) (n2-1) else certainly False
-- only minimal smaller
palindromeEfficient3 :: Int -> Dist Bool
palindromeEfficient3 n = palindrome' 1 n
 where palindrome' n1 n2  | n1 == n2   = pickChar >>>= \ c -> certainly True
                          | n1 > n2    = certainly True
                          | otherwise  = pickChar >>>= \c1 ->
                                         pickChar >>>= \c2 ->
                                         palindrome' (n1+1) (n2-1) >>>= \ b ->
                                         certainly (c1 == c2 && b)

{-
tplp> fst ?? palindromeEfficient 5
Evaluating expression: fst ?? palindromeEfficient 5
0.25

real    0m0.022s
user    0m0.003s
sys     0m0.006s


tplp> fst ?? palindromeEfficient 10
Evaluating expression: fst ?? palindromeEfficient 10
3.125e-2

real    0m0.024s
user    0m0.005s
sys     0m0.006s


tplp> fst ?? palindromeEfficient 20
Evaluating expression: fst ?? palindromeEfficient 20
9.765625e-4

real    0m0.156s
user    0m0.134s
sys     0m0.016s


tplp> fst ?? palindromeEfficient 25
Evaluating expression: fst ?? palindromeEfficient 25
2.44140625e-4

real    0m0.797s
user    0m0.736s
sys     0m0.053s


tplp> fst ?? palindromeEfficient 30
Evaluating expression: fst ?? palindromeEfficient 30
3.0517578125e-5

real    0m6.080s
user    0m5.714s
sys     0m0.305s


tplp> fst ?? palindromeEfficient 35
Evaluating expression: fst ?? palindromeEfficient 35
7.62939453125e-6

real    0m31.322s
user    0m28.972s
sys     0m1.595s
-}