module Sat ( davisPutnam
           , subtraction
           , walkthru
           , walksub
           , genSat
           ) where

-- We opt for DIMACS[5] representation as in  the reader
type Literal = Integer
type Clause  = [Literal]
type Sentence  = [Clause]

-- Specialized data types for the sub and walksub
type BoolVector = [Literal]
type BoolSpace  = [BoolVector]

--- Algorithms ---

davisPutnam :: Sentence -> Bool
davisPutnam s | null s     = True
              | hasEmpty s = False
              | otherwise  = or . map davisPutnam $ assign s

assign :: Sentence -> [Sentence]
assign s@((x:xs):ys) = [(prune (abs x) s), (prune (negate (abs x)) s)]

--remove clauses with x and then remove +-x from each clause
prune :: Integer -> Sentence -> Sentence
prune x s = map (filter (diffAbs x)) $ filter (x `notElem`) s

diffAbs :: Integer -> Integer -> Bool
diffAbs x y = abs x == abs y

hasEmpty :: Sentence -> Bool
hasEmpty s = or $ map null s

subtraction :: Sentence -> Bool
subtraction = error "undefined"

walkthru    :: Sentence -> Bool
walkthru    = error "undefined"

walksub     :: Sentence -> Bool
walksub     = error "undefined"

-- Exploit Lazyness to get a stream of Sat Sentences with "a" Literals
genSat :: Integer -> [Sentence]
genSat a = error "undefined"
