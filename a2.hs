import
  Data.List

scoreMatch = 0
scoreMismatch = (-1)
scoreSpace = (-1)

type AlignmentType = (String, String)

score :: Char -> Char -> Int
score x y
  | x == '-' || y == '-' = scoreSpace
  | x == y               = scoreMatch
  | x /= y               = scoreMismatch

optSimilarityScore :: String -> String -> Int -- function takes in 2 strings and returns an int (the similarity score)
optSimilarityScore string1 string2 = optLen (length string1) (length string2) --the last cell in the optlen thing (table) points to the score of both strings (not substrings or whatevs)
  where
    --string1 = reverse string1
    --string2 = reverse string2 --not too sure if I need to reverse the input strings here like I did for the newOptAlignments function. Reversed or not, the two strings should return the same score anyway...
                                -- but because specific cells in this table are being referenced in the newOptAlignments function, that might be whats causing the missing alignments..?

    optLen :: Int -> Int -> Int
    optLen i j = optTable!!i!!j --optLen points/references a cell in optTable which is a table which actually computes and holds all the scores
    optTable = [[optEntry i j | j<-[0..]] | i<-[0..]] --filling up the table with values as computed by the optEntry function

    optEntry :: Int -> Int -> Int
    optEntry 0 0 = 0 --obvs 2 empty lists have a score of 0
    optEntry i 0 = scoreSpace * i --because all substrings of string one are compared to the empty list (j) its the equlivalent of comparing to a list of spaces of the same size
    optEntry 0 j = scoreSpace * j --ditto
    optEntry i j = maximum [score x y + optLen (i-1) (j-1), --adding to the score that's to the top left of current cell
                            score '-' y + optLen i (j-1), --adding to the score above current cell
                            score x '-' + optLen (i-1) j] --adding to the score to the left of current cell
                            --we want to pick the maximum coz we want to build up the cells with the highest value possible
                            --while taking into consideration all the combinations of spaces within each input strings

      where x = string1!!(i-1) -- just getting the heads of the current substring for score comparison purposes
            y = string2!!(j-1) -- ditto

duplicate :: String -> Int -> String --just a helper function for the next optAlignments function
duplicate string n = concat $ replicate n string --example duplicate "-" 3 will return "---"

newOptAlignments :: String -> String -> [AlignmentType]
newOptAlignments string1 string2 = map (\(a, b) -> (reverse a, reverse b)) (snd(algLen (length string1) (length string2))) --function takes in 2 strings, reverses them?? (I don't actually know coz I copied this line)
                                                                                                                           --opt alignments are stored in the second part of the algLen tuple as these tuples take the form
                                                                                                                           -- (int, [alignments]) hence the 'snd' and algLen (length string1) (length string2) references
                                                                                                                           --the score and alignments of the whole of both string1 and string2 strings i.e. no substrings etc
  where
    algLen :: Int -> Int -> (Int, [AlignmentType])
    algLen i j = algTable!!i!!j --the algLen row and column arguments refer to its corresponding cell in the algTable
    algTable = [[algEntry i j | j<-[0..]] | i<-[0..]] --filling up the algTable with values as computed by the algEntry function

    algEntry :: Int -> Int -> (Int, [AlignmentType])
    algEntry 0 0 = (0, [([],[])]) --obvs the two empty lists will have a score of 0 and no alignments
    algEntry i 0 = (optSimilarityScore (show(take i string1)) (duplicate "-" i), [(show(take i string1), duplicate "-" i)]) --the score part of the tuple is extracted from the similarity score table as defined in optSimilarity score table
                                                                                                                            --and the alignments are going to be each substring of string1 compared with a string full of spaces
                                                                                                                            --ie (["c"], ["-"]) and (["ca"], ["--"]) and (["cat"], ["---"])
    algEntry 0 j = (optSimilarityScore (duplicate "-" j) (show(take j string2)), [(duplicate "-" j, show(take j string2))]) --ditto
    algEntry i j = (fst (head alignments), concat $ map snd alignments) --so the similarity score and alignments are extracted from the alignments variable (as defined below) which has been put together by the maximaBy function
        where
            (scoreDiag, alignmentDiag) = algLen (i-1) (j-1) --the following variables extract the scores and alignments of the neighbouring cells so we can build on those scores and alignments as we progress through longer substrings
            (scoreLeft, alignmentLeft) = algLen (i-1) j --and these values are used in the maximaBy function to generate optimal alignments
            (scoreAbove, alignmentAbove) = algLen i (j-1)

            x = string1!!(i-1) --just gets the current head of the substrings of string1 and string2
            y = string2!!(j-1)
            alignments = maximaBy fst $ [(scoreDiag + score x y, attachHeads x y alignmentDiag),
                                   (scoreLeft + score x '-', attachHeads x '-' alignmentLeft),
                                   (scoreAbove + score '-' y, attachHeads '-' y alignmentAbove)] --so the scores from a) the current substring heads b) string1 head and a space c) the string2 head and a space are calculated
                                                                                                 --and added to their respective neighbouring cells as well as attaching those heads (and spaces) to their neighbouring cell alignments too
                                                                                                 --this means that we are building up all possible alignments as we progress through the table
                                                                                                 --maximaBy fst will return all (score, [alignments]) which have the highest similarityScore value so far
                                                                                                 --then this result is then used to fill all remaining algEntry i j cells in the algTable

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
  where result = newOptAlignments string1 string2
        numResults = length result
        firsts = map (intersperse ' ') $ map fst result
        seconds = map (intersperse ' ') $ map snd result
        newResult = zip firsts seconds
        formatResult = [ x ++ "\n" ++ y ++ "\n\n" | (x,y)<-newResult ]
