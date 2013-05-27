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

--- Algs ---


davisPutnam :: Sentence -> Bool
davisPutnam = error "undefined"

subtraction :: Sentence -> Bool
subtraction = error "undefined"

walkthru    :: Sentence -> Bool
walkthru    = error "undefined"

walksub     :: Sentence -> Bool
walksub     = error "undefined"

-- Exploit Lazyness to get a stream of Sat Sentences with "a" Literals
genSat :: Integer -> [Sentence]
genSat a = error "undefined"
