import
  Data.List

scoreMatch = 0
scoreMismatch = (-1)
scoreSpace = (-1)

type AlignmentType = (String, String)


-- Response to question 1.
-- If we had access to an algorithm for the string alignment problem,
-- we could simply set different values for the scoring mechanism, and 
-- it would be equivalent. For example, if we set scoreMatch to 1, and
-- scoreSpace or scoreMismatch, it would return the largest common subsequence

-- Scoring mechanism used to determine value of a string
score :: Char -> Char -> Int
score x y
  | x == '-' || y == '-' = scoreSpace
  | x == y               = scoreMatch
  | x /= y               = scoreMismatch

-- 2.a) Calculates optimal score for two input strings
usimilarityScore :: (String, String) -> Int
usimilarityScore ([], []) = 0
usimilarityScore ([], (y:ys)) = usimilarityScore ([], ys) + score '-' y
usimilarityScore ((x:xs), []) = usimilarityScore (xs, []) + score x '-'
usimilarityScore ((x:xs), (y:ys)) =
  maximum [(usimilarityScore(xs, ys) + score x y),
    (usimilarityScore(xs, (y:ys)) + score x '-'),
    (usimilarityScore((x:xs), ys) + score '-' y)]
  


-- 2.b) This function takes two arguments, and respectively attaches them to the front
-- of a pair of two lists, for every pair in the list of pairs
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

-- 2.c) Determines highest value present in list, and filters list
-- based on that high value
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs =
  filter (\x -> valueFcn x == highestVal) xs
  where highestVal = head . last . group . sort . map valueFcn $ xs


-- 2.d) Returns a list of all optimal alignments between string1 and string 2
uoptAlignments :: String -> String -> [AlignmentType]
uoptAlignments string1 string2 =
  maximaBy calcScore $ generate string1 string2
  where calcScore :: (String, String) -> Int
        calcScore ([], []) = 0
        calcScore ((x:xs), (y:ys))
          | x == '-' || y == '-' = scoreSpace + calcScore(xs, ys)
          | x == y               = scoreMatch + calcScore(xs, ys)
          | x /= y               = scoreMismatch + calcScore(xs, ys)

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



-- 2.e) Function that outputs best possible arrangements in
-- readable fashion
uoutputOptAlignments :: String -> String -> IO()
uoutputOptAlignments string1 string2 =
  putStrLn ("There are " ++ show (numResults)
  ++ " optimal alignments: \n\n" ++ concat formatResult
  ++ "There were " ++ show (numResults) ++ " optimal alignments!")
  where result = uoptAlignments string1 string2
        numResults = length result
        firsts = map (intersperse ' ') $ map fst result
        seconds = map (intersperse ' ') $ map snd result
        newResult = zip firsts seconds
        formatResult = [ x ++ "\n" ++ y ++ "\n\n" | (x,y)<-newResult ]


-- 3. ) Optimized versions of previous functions
similarityScore :: String -> String -> Int
similarityScore string1 string2 = optLen (length string1) (length string2) 
  where
    optLen :: Int -> Int -> Int
    optLen i j = optTable!!i!!j
    optTable = [[optEntry i j | j<-[0..]] | i<-[0..]]

    optEntry :: Int -> Int -> Int
    optEntry 0 0 = 0
    optEntry i 0 = scoreSpace * i
    optEntry 0 j = scoreSpace * j
    optEntry i j = maximum [score x y + optLen (i-1) (j-1),
                            score '-' y + optLen i (j-1),
                            score x '-' + optLen (i-1) j]

      where x = string1!!(i-1)
            y = string2!!(j-1)

optAlignments :: String -> String -> [AlignmentType]
optAlignments string1 string2 = map (\(a,b) -> (reverse a, reverse b)) $ snd $ algLen (length string1) (length string2)
  where
    algLen :: Int -> Int -> (Int, [AlignmentType])
    algLen i j = algTable!!i!!j
    algTable = [[algEntry i j | j<-[0..]] | i<-[0..]]

    algEntry :: Int -> Int -> (Int, [AlignmentType])
    algEntry 0 0 = (0, [([],[])])
    algEntry i 0 = 
      let (a, b) = algLen(i-1) 0
          x = string1!!(i-1) 
      in (scoreSpace + a, (attachHeads x '-' b))
    algEntry 0 j = 
      let (a, b) = algLen 0 (j-1) 
          y = string1!!(j-1)
      in (scoreSpace + a, (attachHeads '-' y b))

    algEntry i j = (fst $ head alignments, concat $ map snd alignments)
      where
        alignments = maximaBy fst [diag, left, above]

        diag = (scoreDiag + score x y, attachHeads x y alignmentDiag)
        left = (scoreLeft + score x '-', attachHeads x '-' alignmentLeft)
        above = (scoreAbove + score '-' y, attachHeads '-' y alignmentAbove)

        (scoreDiag, alignmentDiag) = algLen (i-1) (j-1)
        (scoreLeft, alignmentLeft) = algLen (i-1) j
        (scoreAbove, alignmentAbove) = algLen i (j-1)

        x = string1!!(i-1)
        y = string2!!(j-1)

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
