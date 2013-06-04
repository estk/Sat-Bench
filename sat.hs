module Sat ( davisPutnam
           , subtraction
           , walkthru
           , walksub
           , genSat
           ) where

import Data.List
import Data.Ord
import Data.Function
import Control.Applicative

-- We opt for DIMACS[5] representation as in  the reader
type Literal = Integer
type Clause  = [Literal]
type Sentence  = [Clause]

-- Specialized data types for the sub and walksub
type BoolVector = [Literal]
type BoolSpace  = [BoolVector]

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
resolution = foldl subClause [[0]]

subClause :: SS -> Clause -> SS
subClause ss c = concatMap (res c) ss

res :: Clause -> SClause -> SS
res c sc = case foldl match ([], sc, c) c of
            (ss, _, _) -> ss

match :: (SS, SClause, Clause) -> Literal -> (SS, SClause, Clause)
match (solns, s, c) l = case samePairity l s of
                 Just True  -> (s:solns, s, c)
                 Just False -> (solns, s, c)
                 Nothing    -> (((negLeftL l c)++[l]):solns, s, c)

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

data Match = Match | Fail Integer

walkthru    :: Sentence -> Bool
walkthru    = or . map walk . permutations
    
walk :: Sentence -> Bool
walk s = case foldl matchW (Just []) s of -- accumulate a solution
           Nothing -> False
           Just _  -> True

{-Nothing => Fail, Just SClause => SClause matches c-}
{-matchW :: Maybe SClause -> Clause -> Maybe SClause-}
{-matchW Nothing _ = Nothing-}
{-matchW (Just sc) c = case foldl (matcher c) (Fail 0) sc of-}
               {-Match   -> Just sc-}
               {-Fail l  -> (sortBy (compare `on` abs)) <$> (findExtFor l sc c)-}

{-matcher :: Clause -> Match -> Literal -> Match-}
{-matcher _ Match _ = Match-}
{-matcher c _ l     = if l `elem` c-}
                    {-then Match  -}
                    {-else Fail l -}

{-findExtFor :: Literal -> SClause -> Clause -> Maybe SClause-}
{-findExtFor l sc c = case filter ((> abs l) . abs) c of-}
                      {-[]  -> Nothing-}
                      {-lst -> Just (sc ++ [minAbs lst])-}


matchW :: Maybe SClause -> Clause -> Maybe SClause
matchW Nothing _ = Nothing
matchW (Just sc) c = if sc `satisfies` c
               then Just sc
               else sc `findExtFor` c

satisfies :: SClause -> Clause -> Bool
satisfies sc c = any (`elem` c) sc

findExtFor :: SClause -> Clause -> Maybe SClause
findExtFor sc c = case rmLeftEq (maxAbs sc) c of
                       [] ->  Nothing
                       lst -> Just ((minAbs lst):sc)

rmLeftEq :: Literal -> Clause -> Clause
rmLeftEq l c = filter ((> abs l) . abs) c

maxAbs :: SClause -> Literal
maxAbs sc = foldl mAbs 0 sc
  where mAbs m x = if (abs m) > (abs x)
                     then m
                     else x

minAbs :: Clause -> Literal
minAbs sc = foldl mAbs (head sc) sc
  where mAbs m x = if (abs m) < (abs x)
                     then m
                     else x


-------------
-- Walksub --
-------------
        
walksub     :: Sentence -> Bool
walksub     = error "undefined"

-- Exploit Lazyness to get a stream of Sat Sentences with "a" Literals
genSat :: Integer -> Integer -> Sentence
genSat n m = error "undefined"
