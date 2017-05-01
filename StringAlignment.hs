scoreMatch = 0
scoreMismatch = (-1)
scoreSpace = (-1)

score :: (Char,Char) -> Int
score (x, y) 
  | x == '-' || y == '-' = scoreSpace
  | x == y               = scoreMatch
  | x /= y               = scoreMismatch


similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore [] (y:ys) = similarityScore [] ys + score('-',y)
similarityScore (x:xs) [] = similarityScore xs [] + score(x,'-')
similarityScore (x:xs) (y:ys) =
  let first = similarityScore xs ys + score(x,y)
      second = similarityScore xs (y:ys) + score(x,'-')
      third = similarityScore (x:xs) ys + score('-',y)
  in max first $ max second third
  


