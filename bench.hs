import Criterion.Main
import Sat

sat = gen3Sats
sat1 = take 10 sat
sat2 = take 100 sat
sat3 = take 1000 sat

main = defaultMain [
       bgroup "genSat"
                      [ bench "10"   $ whnf null sat1 
                      , bench "100"  $ whnf null sat2 
                      , bench "1000" $ whnf null sat3 
                      ]
                   ,
       bgroup "davisPutnam"
                      [ bench "10"   $ whnf davisPutnam sat1
                      , bench "100"  $ whnf davisPutnam sat2
                      , bench "1000" $ whnf davisPutnam sat3
                      ]
                   ,
       bgroup "subtraction"
                      [ bench "10"   $ whnf subtraction sat1
                      , bench "100"  $ whnf subtraction sat2
                      , bench "1000" $ whnf subtraction sat3
                      ]
                   ,
       bgroup "walkthru"
                      [ bench "10"   $ whnf walkthru sat1
                      , bench "100"  $ whnf walkthru sat2
                      , bench "1000" $ whnf walkthru sat3
                      ]
                   ,
       bgroup "walksub"
                      [ bench "10"   $ whnf walksub sat1
                      , bench "100"  $ whnf walksub sat2
                      , bench "1000" $ whnf walksub sat3
                      ]
                   ]

