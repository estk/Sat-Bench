module Sat ( davisPutnam
           , subtraction
           , walkthru
           , walksub
           , genSats
           , genUnSats
           ) where

import Surely
import Data.Maybe
import Data.List
import Data.Ord
import Data.Function
import Control.Applicative
import System.Random
import System.IO.Unsafe

-- We opt for DIMACS[5] representation as in  the reader
type Literal = Integer
type Clause  = [Literal]
type Sentence  = [Clause]

--- Algorithms ---

------------------
-- Davis Putnam --
------------------

davisPutnam :: Sentence -> Bool
davisPutnam s | null s     = True
              | hasEmpty s = False
              | otherwise  = or . map davisPutnam $ assignBranch s

assignBranch :: Sentence -> [Sentence]
assignBranch s@((x:xs):ys) = [(assign (abs x) s), (assign (negate (abs x)) s)]

--remove clauses with x and then remove +-x from each clause
assign :: Integer -> Sentence -> Sentence
assign l s = map (rmLiteral l) $ foldr f [] s
  where f c a | l `notElem` c = rmLiteral l c : a
              | otherwise     = a

rmLiteral :: Literal -> Clause -> Clause
rmLiteral l = filter ((/= abs l) . abs)

hasEmpty :: Sentence -> Bool
hasEmpty = or . map null

-----------------
-- Subtraction --
-----------------

type SS = Sentence
type SClause = Clause
type SLiteral = Literal

subtraction :: Sentence -> Bool
subtraction = not . null . resolution

-- need a non-null clause to start with
resolution :: Sentence -> SS
resolution s = foldl subClause [[0]] s

subClause :: SS -> Clause -> SS
subClause ss c = concatMap (res c) ss

res :: Clause -> SClause -> SS
res c sc = if not . null $ c `intersect` sc
             then [sc] --most general sclause matches
             else addin sc $ filter (not . (`absElem` sc)) c --consider free vars

--Take a SClause and literals which can be assigned such that they will match the clause specified to the caller. 
addin :: SClause -> [Literal] -> SS
addin sc lst = map (++ sc) (addends [] lst)

        
addends :: [Literal] -> [Literal] ->[[Literal]]
addends added []   = []
addends added lst =   (minAbsRest : (map negate $ delete minAbsRest added))
                    : (addends (minAbsRest:added) $ (delete minAbsRest) lst)
  where minAbsRest = minAbs $ lst \\ added

absElem :: (Num a, Eq a) => a -> [a] -> Bool
absElem a lst = abs a `elem` map abs lst

match :: (SS, SClause, Clause) -> Literal -> (SS, SClause, Clause)
match (ss, s, c) l = case samePairity l s of
                 Just True  -> (s:ss, s, c)
                 Just False -> (ss, s, c)
                 Nothing    -> ((negLeftL l c ++ [l]):ss, s, c)

-- Nothing => not in, Just True => Same pairity, Just False => opposite
samePairity :: Literal -> SClause -> Maybe Bool
samePairity l s = case filter ((== abs l) . abs) s of
                    []  -> Nothing
                    lst -> Just $ l `elem` lst

negLeftL :: Literal -> Clause -> SClause
negLeftL l = map negate . filter ((< abs l) . abs)


--------------
-- Walkthru --
--------------

walkthru :: Sentence -> Bool
walkthru s = walk s []
    
walk :: Sentence -> [Sentence] -> Bool
walk s p = if s `elem` p
             then False
             else  case walkh of
                    Right c -> walk (c:(delete c s)) (s:p)
                    Left _  -> True
  where walkh = foldl dispatchW (Left []) s -- accumulate a SClause

-- propagate Right (failure clause)
dispatchW :: Either SClause Clause -> Clause -> Either SClause Clause
dispatchW e c = either (`matchW` c) Right e

-- see if sc matches c
-- Fail => Right Failed Clause, Left adapted sc (to match c)
matchW :: SClause -> Clause -> Either SClause Clause
matchW sc c = if sc `satisfies` c
               then Left sc
               else sc `findExtFor` c

satisfies :: SClause -> Clause -> Bool
satisfies sc c = any (`elem` c) sc

findExtFor :: SClause -> Clause -> Either SClause Clause
findExtFor sc c = case rmLeftEq (maxAbs sc) c of
                       [] ->  Right c
                       lst -> Left (minAbs lst:sc)

rmLeftEq :: Literal -> Clause -> Clause
rmLeftEq l = filter ((> abs l) . abs)

maxAbs :: SClause -> Literal
maxAbs = foldl mAbs 0
  where mAbs m x = if abs m > abs x
                     then m
                     else x

-- sc cannot have an empty head
minAbs :: Clause -> Literal
minAbs sc = foldl mAbs (head sc) sc
  where mAbs m x = if abs m < abs x
                     then m
                     else x


-------------
-- Walksub --
-------------

walksub :: Sentence -> Bool
walksub = dispatch [0]

-- see if by starting with sc, we can build a matching SClause
dispatch :: SClause -> Sentence -> Bool
dispatch sc s | null s = False
              | null sc = False
              | otherwise =
                case walker sc s of
                  Left _  -> True
                  Right c -> or . map (`dispatch` (delete c s)) $ subber c sc

-- see if by starting with sc, we can build a matching SClause
-- Fail -> Left Failure Clause, Success -> Satisfying Clause
walker :: SClause -> Sentence -> Either SClause Clause
walker sc s = foldl dispatchW (Left sc) s

-- Takes clause where walker failed, subtracts it, builds a list of pairs
-- like so [(Partial SClause, Sentence to match against)]
subber :: Clause -> SClause -> SS
subber c sc = res c sc

-------------------
-- Generate Sats --
-------------------


genUnSats :: RandomGen g => Int -> Integer -> g -> Sentence
genUnSats m n g = (take m $ genSats m n g) ++ [[]]

-- Exploit Lazyness to get a stream of Sat Sentences with "a" Literals
genSats :: RandomGen g => Int -> Integer -> g -> Sentence
genSats m n g = take m $ genSatHelp n (randomRs (-n,n) g)

genSatHelp :: Integer -> [Integer] -> Sentence
genSatHelp n rs = makeClause : (genSatHelp n $ drop (fromIntegral n) rs)
  where eqAbs x y = abs x == abs y
        makeClause = delete 0 $ nubBy eqAbs $ take (fromIntegral n) rs


-- We run each alg over a set of 3-Sat sentences of 5 to 20 clauses in
-- lenght. Solve is imported from an outside project as to insure
-- correctness.
test :: Bool
test = null . tail . nub $ nakedTest

nakedTest = map dm algs
  where datas = map (\n -> (genSats n 3 (mkStdGen 1))) [5..20] 
        algs  = [(isJust . solve), davisPutnam, subtraction, walkthru, walksub]
        dm alg = map alg datas
