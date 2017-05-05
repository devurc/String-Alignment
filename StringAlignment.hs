import 
  Data.List 

scoreMatch = 0
scoreMismatch = (-1)
scoreSpace = (-1)

type AlignmentType = (String,String)

score :: Char -> Char -> Int
score x y
  | x == '-' || y == '-' = scoreSpace
  | x == y               = scoreMatch
  | x /= y               = scoreMismatch


-- 2.a) Calculates optimal score for two input strings
similarityScore :: (String, String) -> Int
similarityScore ([], []) = 0
similarityScore ([], (y:ys)) = similarityScore ([], ys) + score '-' y
similarityScore ((x:xs), []) = similarityScore (xs, []) + score x '-'
similarityScore ((x:xs), (y:ys)) =
  let first = similarityScore(xs, ys) + score x y
      second = similarityScore(xs, (y:ys)) + score x '-'
      third = similarityScore((x:xs), ys) + score '-' y
  in max first $ max second third
  

-- 2.b) This function takes two arguments, and respectively attaches them to the front 
-- of a pair of two lists, for every pair in the list of pairs
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

-- 2.c) Determines highest value present in list, and filters list
-- based on that high value
maximaBy :: Ord b => (a -> b) -> [a] -> [a] 
maximaBy valueFcn xs =
  -- [ x | x<-xs, valueFcn x == highestVal ]
  filter (\x -> valueFcn x == highestVal) xs
  where highestVal = head . last . group . sort . map valueFcn $ xs

-- 2.d) Returns a list of all optimal alignments between string1 and string 2
optAlignments :: String -> String -> [AlignmentType]
optAlignments string1 string2 = 
  maximaBy calcScore $ generate string1 string2

-- Helper function that generates all possible alignments of 
-- a pair of two strings
generate :: String -> String -> [AlignmentType]
generate [] []     = [([], [])]
generate (x:xs) [] = attachHeads x '-' $ generate xs []
generate [] (y:ys) = attachHeads '-' y $ generate [] ys
generate (x:xs) (y:ys) = 
  attachHeads x y (generate xs ys) ++ 
  attachHeads x '-' (generate xs (y:ys)) ++ 
  attachHeads '-' y (generate (x:xs) ys)


-- Helper function that calculates similarity score for a given
-- pair of strings
calcScore :: (String, String) -> Int
calcScore ([], []) = 0
calcScore ((x:xs), (y:ys))
  | x == '-' || y == '-' = scoreSpace + calcScore(xs, ys)
  | x == y               = scoreMatch + calcScore(xs, ys)
  | x /= y               = scoreMismatch + calcScore(xs, ys)


-- 2.e) Function that outputs best possible arrangements in
-- readable fashion
outputOptAlignments :: String -> String -> IO()
outputOptAlignments string1 string2 =
  putStrLn ("There are " ++ show (numResults)
  ++ " optimal alignments: \n\n" ++ concat formatResult
  ++ "There were " ++ show (numResults) ++ " optimal alignments!")
  where result = optAlignments string1 string2
        numResults = length result
        firsts = map (intersperse ' ') $ map fst result
        seconds = map (intersperse ' ') $ map snd result
        newResult = zip firsts seconds
        formatResult = [ x ++ "\n" ++ y ++ "\n\n" | (x,y)<-newResult ]


