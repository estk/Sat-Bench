module Sat ( davisPutnam
           , subtraction
           , walkthru
           , walksub
           , gen3Sats
           ) where

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

--take a SClause and literals which can be assigned such that they will match the clause specified to the caller. 
addin :: SClause -> [Literal] -> SS
addin sc lst = map (++ sc) (addends [] lst)

        
addends :: [Literal] -> [Literal] ->[[Literal]]
addends added []   = []
addends added lst =   (minAbsRest : (map negate $ delete minAbsRest added))
                    : (addends (minAbsRest:added) $ (delete minAbsRest) lst)
  where minAbsRest = minAbs $ lst \\ added

absElem :: (Num a, Eq a) => a -> [a] -> Bool
absElem a lst = abs a `elem` map abs lst
{-case foldl match ([], sc, c) c of-}
            {-(ss, _, _) -> ss-}

match :: (SS, SClause, Clause) -> Literal -> (SS, SClause, Clause)
match (ss, s, c) l = case samePairity l s of
                 Just True  -> (s:ss, s, c)
                 Just False -> (ss, s, c)
                 Nothing    -> ((negLeftL l c ++ [l]):ss, s, c)

-- Nothing => not in, Just True => Same pairity, Just False => opposite
-- pairity
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
             else 
               case foldl matchW (Left []) s of -- accumulate a solution
                 Right c -> walk (c:(delete c s)) (s:p)
                 Left _  -> True

matchW :: Either SClause Clause -> Clause -> Either SClause Clause
matchW (Right c) _ = Right c
matchW (Left sc) c = if sc `satisfies` c
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

minAbs :: Clause -> Literal
minAbs sc = foldl mAbs (head sc) sc
  where mAbs m x = if abs m < abs x
                     then m
                     else x


-------------
-- Walksub --
-------------
        
walksub     :: Sentence -> Bool
walksub s = case ws s of -- accumulate a solution
           Nothing -> False
           Just _  -> True

ws :: Sentence -> Maybe SClause
ws = foldl (matchWS [[0]]) (Just [])

matchWS :: SS -> Maybe SClause -> Clause -> Maybe SClause
matchWS _ Nothing _    = Nothing
matchWS ss (Just sc) c = if sc `satisfies` c
               then Just sc
               else error "unwritten"


-- Exploit Lazyness to get a stream of Sat Sentences with "a" Literals
gen3Sats :: Sentence
gen3Sats = genSatHelp rs
    where g = unsafePerformIO getStdGen
          rs = randomRs (-3,3) g
genSats _ = error "genSat is unwritten"

genSatHelp :: [Integer] -> Sentence
genSatHelp rs = makeClause : (genSatHelp $ drop 3 rs)
  where eqAbs x y = abs x == abs y
        makeClause = delete 0 $ nubBy eqAbs $ take 3 rs

test = (unsafePerformIO $ print $ head datas) `seq` map dm algs
  where datas = [take 5 gen3Sats, take 100 gen3Sats, take 1000 gen3Sats]
        algs  = [davisPutnam, subtraction, walkthru]
        dm alg = map alg datas
