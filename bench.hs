import Criterion.Main
import Sat

sat = genSat 3

main = defaultMain [
       bgroup "davisPutnam"
                      [ bench "10" $ whnf davisPutnam $ sat 10
                      , bench "20" $ whnf davisPutnam $ sat 20
                      , bench "30" $ whnf davisPutnam $ sat 30
                      ]
                   ,
       bgroup "subtraction"
                      [ bench "10" $ whnf subtraction $ sat 10
                      , bench "20" $ whnf subtraction $ sat 20
                      , bench "30" $ whnf subtraction $ sat 30
                      ]
                   ,
       bgroup "walkthru"
                      [ bench "10" $ whnf walkthru $ sat 10
                      , bench "20" $ whnf walkthru $ sat 20
                      , bench "30" $ whnf walkthru $ sat 30
                      ]
                   {-,-}
       {-bgroup "walksub"-}
                      {-[ bench "10" $ whnf walksub sat-}
                      {-, bench "20" $ whnf walksub sat-}
                      {-, bench "30" $ whnf walksub sat-}
                      {-]-}
                   ]

