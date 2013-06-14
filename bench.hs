import Criterion.Main
import Sat
import Surely
import Control.Monad

main = do
    sat1 <- liftM (take 10) gen3Sats
    sat2 <- liftM (take 100) gen3Sats    
    sat3 <- liftM (take 1000) gen3Sats    
    return $ defaultMain [
       bgroup "genSat"
                      [ bench "10"   $ whnf null sat1 
                      , bench "100"  $ whnf null sat2 
                      , bench "1000" $ whnf null sat3 
                      ]
                   ,
       bgroup "surely"
                      [ bench "10"   $ whnf solve sat1
                      , bench "100"  $ whnf solve sat2
                      , bench "1000" $ whnf solve sat3
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

