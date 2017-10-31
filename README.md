# String-Alignment
Project 2 for EDAN40 - Functional Programming

A major part of molecular biology deals with analyzing and comparing the genetic material (DNA, RNA, proteins, chromosomes, genes, etc.) of different organisms. Enormous amounts of information have to be collected, stored, and processed; fortunately, many kinds of biomolecular data share a common underlying sequential structure. For instance, the primary structure of a protein is a one-dimensional chain of amino acid residues, and hence expressible as a string over a finite alphabet. In the same way, the linear sequence of bases in a DNA molecule can be represented by a string over the alphabet {A,C,G,T}, and RNA by a string over the alphabet {A,C,G,U}. Efficient string algorithms have therefore turned out to be of great importance for solving combinatorial problems related to modern molecular biology.

One example of such a problem is the string alignment problem. An alignment of two strings is a way of placing one string above the other to illustrate how parts of the strings are related. Given two strings s and t, an alignment is obtained by inserting spaces into s and t so that the characters of the resulting strings can be put in one-to-one correspondence to each other, as in the following example where the symbol '-' is used to represent an inserted space:

| H A S K E L L
| P A S C A - L

Spaces may also be added to the beginning and the end of the strings, but a space in one string is never allowed to be aligned with a space in the other (for obvious reasons). Thus, another valid way to align the two strings is:

| H - A S K E L L
| - P A S C - A L

The length of an alignment is the number of columns it contains, so the first alignment has length 7 while the second alignment has length 8. Looking more closely, we see that in the first alignment, there are 3 matches, 3 mismatches, and 1 space; in the second, there are 3 matches, 2 mismatches, and 3 spaces. Which of the two alignments is "better"?

There is no definite answer, as this depends on the application, i.e., on how we choose to penalize mismatches and inserted spaces. This means that in some cases, the first alignment would be considered better than the second, while the second alignment would be preferred in other cases. The parameters that specify the optimization criteria will be referred to as scoreMatch, scoreMismatch, and scoreSpace.

One application of the string alignment technique is to identify an unknown virus. By aligning its RNA sequence to various known RNA sequences stored in a database (one at a time), and rejecting those with a similarity score lower than a certain threshold, one can select a small set of candidate RNA sequences to be subjected to further, more detailed comparisons. Another use of string alignments is when different laboratories that are working on obtaining the DNA sequence of a certain gene want to compare their results, an alignment will show where their results differ. Finally, alignments between DNA sequences from a set of similar species provide an indication of how the species are related to each other. This information can then be used to construct an evolutionary tree which describes how they have evolved from an assumed common ancestor. There are many other uses of string alignments in molecular biology as well as in other fields such as computer science, coding theory, dendrochronology, stratigraphic analysis, forensics, and speech recognition.

The difficulty involved in aligning strings is that the total number of possible alignments is exponential in the lengths of the input strings. A brute-force approach (generate all possible alignments, evaluate each one, and return all those with the highest score) is therefore useless for strings of sizes that occur in practice. For example, there are more than 10^764 possible alignments for two strings of length 1000. But it is possible to use a bottom-up method that takes advantage of the structure of the problem, and in this assignment, you will write a Haskell program that solves the string alignment problem more efficiently.

# Problem specification
Formally, the string alignment problem is defined as follows.

Input: Two strings s and t, and values for scoreMatch, scoreMismatch, and scoreSpace.
Output: All optimal alignments between s and t.
Or in terms of a Haskell type signature:

optimalAlignments :: Int -> Int -> Int -> String -> String -> [AlignmentType]
In order to be able to compare different alignments, a scoring scheme is required. For this reason, we define the score of an alignment as the sum of its column scores, where a column score is calculated from the characters in that column. If one of the characters is a space, the column score is equal to scoreSpace; otherwise, it is equal to either scoreMatch or scoreMismatch depending on whether the characters are identical or not. 
In the example above, if we let

scoreMatch = 1
scoreMismatch = -1
scoreSpace = -2
then the score of the first alignment is -2 and the score of the second alignment is -5.

An optimal alignment is an alignment with the highest possible score. The score of such an alignment is called the similarity score of the two strings. Note that there can be more than one optimal alignment.
