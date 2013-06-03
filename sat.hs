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

samePairity :: Literal -> SClause -> Maybe Bool
samePairity l s = case filter ((== abs l) . abs) s of
                    [] -> Nothing
                    lst -> Just $ l `elem` lst

negLeftL :: Literal -> Clause -> SClause
negLeftL l = map negate . filter ((< abs l) . abs)

walkthru    :: Sentence -> Bool
walkthru    = error "undefined"

walksub     :: Sentence -> Bool
walksub     = error "undefined"

-- Exploit Lazyness to get a stream of Sat Sentences with "a" Literals
genSat :: Integer -> Integer -> Sentence
genSat n m = error "undefined"
