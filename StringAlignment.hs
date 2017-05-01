scoreMatch = 1
scoreMismatch = (-1)
scoreSpace = (-2)

score :: (Char,Char) -> Int
score (x, y) 
  | x == '-' || y == '-' = scoreSpace
  | x == y               = scoreMatch
  | x /= y               = scoreMismatch


optimumScore :: (String, String) -> Int
optimumScore ([], []) = 0
optimumScore ([], (y:ys)) = optimumScore([],ys) + score('-',y)
optimumScore ((x:xs), []) = optimumScore(xs,[]) + score(x,'-')
optimumScore ((x:xs),(y:ys)) =
  let first = optimumScore(xs,ys) + score(x,y)
      second = optimumScore(xs, y:ys) + score (x,'-')
      third = optimumScore(x:xs, ys) + score ('-',y)
  in max first $ max second third
  


