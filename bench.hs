import Criterion.Main
import Sat
import Control.Monad
import System.Random

-- Using 10 Literals
gen = mkStdGen 1
sat1 = genSats 10 10 gen
sat2 = genSats 100 10 gen
sat3 = genSats 1000 10 gen

unsat1 = genUnSats 10 10 gen
unsat2 = genUnSats 100 10 gen
unsat3 = genUnSats 1000 10 gen

main = defaultMain [
       bgroup "davisPutnam-sat"
                      [ bench "10"   $ nf davisPutnam sat1
                      , bench "100"  $ nf davisPutnam sat2
                      , bench "1000" $ nf davisPutnam sat3
                      ]
                   ,
       bgroup "davisPutnam-unsat"
                      [ bench "10"   $ nf davisPutnam unsat1
                      , bench "100"  $ nf davisPutnam unsat2
                      , bench "1000" $ nf davisPutnam unsat3
                      ]
                   ,
       bgroup "subtraction-sat"
                      [ bench "10"   $ nf subtraction sat1
                      , bench "100"  $ nf subtraction sat2
                      , bench "1000" $ nf subtraction sat3
                      ]
                   ,
       bgroup "subtraction-unsat"
                      [ bench "10"   $ nf subtraction unsat1
                      , bench "100"  $ nf subtraction unsat2
                      , bench "1000" $ nf subtraction unsat3
                      ]
                   ,
       bgroup "walkthru-sat"
                      [ bench "10"   $ nf walkthru sat1
                      , bench "100"  $ nf walkthru sat2
                      ]
                   ,
       bgroup "walkthru-unsat"
                      [ bench "10"   $ nf walkthru unsat1
                      , bench "100"  $ nf walkthru unsat2
                      ]
                   ,
       bgroup "walksub-sat"
                      [ bench "10"   $ nf walksub sat1
                      , bench "100"  $ nf walksub sat2
                      , bench "1000" $ nf walksub sat3
                      ]
                   ,
       bgroup "walksub-unsat"
                      [ bench "10"   $ nf walksub unsat1
                      , bench "100"  $ nf walksub unsat2
                      , bench "1000" $ nf walksub unsat3
                      ]
                   ]

