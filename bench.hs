import Criterion.Main
import Sat

sat = genSat 3 3

main = defaultMain [
       bgroup "davisPutnam"
                      [ bench "10" $ whnf davisPutnam sat
                      , bench "20" $ whnf davisPutnam sat
                      , bench "30" $ whnf davisPutnam sat
                      ]
                   ,
       bgroup "subtraction"
                      [ bench "10" $ whnf subtraction sat
                      , bench "20" $ whnf subtraction sat
                      , bench "30" $ whnf subtraction sat
                      ]
                   ,
       bgroup "walkthru"
                      [ bench "10" $ whnf walkthru sat
                      , bench "20" $ whnf walkthru sat
                      , bench "30" $ whnf walkthru sat
                      ]
                   ,
       bgroup "walksub"
                      [ bench "10" $ whnf walksub sat
                      , bench "20" $ whnf walksub sat
                      , bench "30" $ whnf walksub sat
                      ]
                   ]

