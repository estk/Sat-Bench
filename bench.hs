import Criterion.Main
import Sat

sats = genSat 3

main = defaultMain [
       bgroup "davisPutnam"
                      [ bench "10" $ whnf davisPutnam (take 10 sats)
                      , bench "20" $ whnf davisPutnam (take 20 sats)
                      , bench "30" $ whnf davisPutnam (take 30 sats)
                      ]
                   ,
       bgroup "subtraction"
                      [ bench "10" $ whnf subtraction (take 10 sats)
                      , bench "20" $ whnf subtraction (take 20 sats)
                      , bench "30" $ whnf subtraction (take 30 sats)
                      ]
                   ,
       bgroup "walkthru"
                      [ bench "10" $ whnf walkthru (take 10 sats)
                      , bench "20" $ whnf walkthru (take 20 sats)
                      , bench "30" $ whnf walkthru (take 30 sats)
                      ]
                   ,
       bgroup "walksub"
                      [ bench "10" $ whnf walksub (take 10 sats)
                      , bench "20" $ whnf walksub (take 20 sats)
                      , bench "30" $ whnf walksub (take 30 sats)
                      ]
                   ]

