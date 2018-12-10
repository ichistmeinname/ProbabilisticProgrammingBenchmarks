{-

You need to set the following import path in the REPL.

    :set path pflp/src/

-}

import System
import PFLP
import List (delete)

type Person = Int
type Hat = [Person]

data SecretSanta = FailedGame | Success [SantaAssignment]
data SantaAssignment = Assignment { santa :: Person, person :: Person }

santaGame :: Int -> Hat
santaGame n | n > 1 = [1..n]
            | otherwise = error "invalid game"

pickFromHat :: Hat -> Dist Person
pickFromHat = uniform

pPicks :: Person -> Hat -> Dist (Maybe (SantaAssignment, [Person]))
pPicks _ [] = certainly Nothing
pPicks p ps@(_:_) =
  pickFromHat ps >>>= \p' ->
  certainly (Just (Assignment p p', delete p' ps))

pPicks' :: Person -> Hat -> Dist (Maybe (SantaAssignment, [Person]))
pPicks' _ [] = certainly Nothing
pPicks' p ps@(_:_) =
  pickFromHat ps >>>= \p' ->
  if p == p' then certainly Nothing else certainly (Just (Assignment p p', delete p' ps))

pickRound :: Hat -> Dist SecretSanta
pickRound [] = certainly FailedGame
pickRound xs@(_:_) = pickRound' xs xs [] >>>= \ arrs -> certainly (Success arrs)
 where
  pickRound' []     _   arrs = certainly arrs
  pickRound' (p:ps) hat arrs =
    pPicks p hat >>>= \ (Just (arr,hat')) ->
    pickRound' ps hat' (arr:arrs)

isFailedAssign :: SantaAssignment -> Bool
isFailedAssign secret = santa secret == person secret

normFailedGame :: SecretSanta -> SecretSanta
normFailedGame FailedGame = FailedGame
normFailedGame game@(Success arrs)  | any isFailedAssign arrs  = FailedGame
                                    | otherwise                = game

santa1 :: Int -> Probability
santa1 n = isFailedGame ?? (pickRound (santaGame n) >>>= \game -> certainly (normFailedGame game))

isFailedGame :: SecretSanta -> Bool
isFailedGame FailedGame    = True
isFailedGame (Success _ )  = False

pickRoundWOFailed :: Hat -> Dist SecretSanta
pickRoundWOFailed [] = certainly FailedGame
pickRoundWOFailed xs@(_:_) = pickRound' xs xs []
 where
  pickRound' []     _   assigns = certainly (Success assigns)
  pickRound' (p:ps) hat assigns =
    pPicks p (delete p hat) >>>= \ mAssign ->
    maybe (certainly FailedGame)
      (\(assign,_) -> pickRound' ps (delete (person assign) hat) (assign:assigns))
      mAssign

santa2 :: Int -> Probability
santa2 n = isFailedGame ?? (pickRoundWOFailed (santaGame n))

pickAndCheckRound :: Hat -> Dist SecretSanta
pickAndCheckRound [] = certainly FailedGame
pickAndCheckRound xs@(_:_) = pickRound' xs xs []
 where
  pickRound' []     _   assigns = certainly (Success assigns)
  pickRound' (p:ps) hat assigns =
    pPicks' p hat >>>= \ mAssign ->
    maybe (certainly FailedGame)
          (\ (assign,newHat) ->
            pickRound' ps newHat (assign:assigns))
          mAssign

pickAndCheckRound' :: Hat -> Dist SecretSanta
pickAndCheckRound' [] = certainly FailedGame
pickAndCheckRound' xs@(_:_) = pickRound' xs xs []
 where
  pickRound' []     _   assigns = certainly (Success assigns)
  pickRound' (p:ps) hat assigns =
    pPicks p hat >>>= \ mAssign ->
    maybe (certainly FailedGame)
          (\ (assign,newHat) ->
            if person assign == p
              then certainly FailedGame
              else pickRound' ps newHat (assign:assigns))
          mAssign

santa3 :: Int -> Probability
santa3 n = isFailedGame ?? (pickAndCheckRound (santaGame n))

santa4 :: Int -> Probability
santa4 n = isFailedGame ?? (pickAndCheckRound' (santaGame n))

main :: IO ()
main = getArgs >>= \args ->
       case args of
         [nArg] -> let n = read nArg
                     in print (santa3 n) >> return ()
         _ : nArg : _  -> let n = read nArg
                           in print (santa4 n) >> return ()
