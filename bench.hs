import Criterion.Main
import Sat

sat = genSat 3

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = defaultMain [
       {-bgroup "naive" [-}
                        {-bench "fib 10" $ whnf fib 10-}
                      {-, bench "fib 20" $ whnf fib 20-}
                      {-, bench "fib 30" $ whnf fib 30-}
                      {-],-}
       bgroup "davisPutnam"
                      [ bench "10" $ whnf davisPutnam (sat 1000)
                      , bench "20" $ whnf davisPutnam (sat 2000)
                      , bench "30" $ whnf davisPutnam (sat 3000)
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

