import Criterion.Main
import Sat
import Control.Monad
import System.Random

-- Using 10 Literals
gen = mkStdGen 1
unsat1 = genUnSats 10 10 gen
unsat2 = genUnSats 100 10 gen
unsat3 = genUnSats 1000 10 gen

main = defaultMain [
       bgroup "davisPutnam"
                      [ bench "10"   $ nf davisPutnam unsat1
                      , bench "100"  $ nf davisPutnam unsat2
                      , bench "1000" $ nf davisPutnam unsat3
                      ]
                   ,
       bgroup "subtraction"
                      [ bench "10"   $ nf subtraction unsat1
                      , bench "100"  $ nf subtraction unsat2
                      , bench "1000" $ nf subtraction unsat3
                      ]
                   ,
       bgroup "walkthru"
                      [ bench "10"   $ nf walkthru unsat1
                      , bench "100"  $ nf walkthru unsat2
                      , bench "1000" $ nf walkthru unsat3
                      ]
                    ,
       bgroup "walksub"
                      [ bench "10"   $ nf walksub unsat1
                      , bench "100"  $ nf walksub unsat2
                      , bench "1000" $ nf walksub unsat3
                      ]
                   ]

