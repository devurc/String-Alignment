scoreMatch = 1
scoreMismatch = (-1)
scoreSpace = (-2)

similarityScore :: String -> String -> Int

similarityScore string1 string2 = score (length string1) (length string2)
    where
        --extracting the similarity score from the table using indexing
        score i j = scoreTable!!i!!j

        --this line creates the score table and simultaneously populates it with the scores
        scoreTable = [[scoreEntry i j | j <- [0..]] | i <- [0..]]


        --All intersections in the table with an empty list has a score of 0
        scoreEntry _ 0 = 0
        scoreEntry 0 _ = 0

        --Else if a list is not empty, then apply the scoring criteria to the two sublists/list
        --Also accumulate score from top left diagonal

        scoreEntry i j
            | h1 == h2 = scoreMatch + score(i-1, j-1)
            | h1 == '_' || h2 == '_' = scoreSpace + score(i-1, j-1)
            | h1 /= h2 = scoreMismatch + score(i-1, j-1)

        --h1 is the head of string1 where
            where
                h1 = string1!!(i-1)
                h2 = string2!!(j-1)
