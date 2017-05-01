import 
  Data.List 

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
  

-- 2.b) This function takes two arguments, and respectively attaches them to the front 
-- of a pair of two lists, for every pair in the list of pairs
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]


maximaBy :: Ord b => (a -> b) -> [a] -> [a] 
maximaBy valueFcn xs =
  -- [ x | x<-xs, valueFcn x == highestVal ]
  filter (\x -> valueFcn x == highestVal) xs
  where highestVal = head . last . group . sort . map valueFcn $ xs









