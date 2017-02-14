import qualified Data.Map as Map
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe

patternToNumber :: String -> Integer
patternToNumber [] = 0
patternToNumber s = 4 * patternToNumber (init s) + symbolToNumber (last s)
  where symbolToNumber c = case c of {'A' -> 0; 'C' -> 1; 'G' -> 2; 'T' -> 3}

numberToPattern :: Int -> Int -> String
numberToPattern n k = pattern "" n k
  where pattern s 0 k = concat (replicate (k - length s) "A") ++ reverse s
        pattern s n k = pattern (s ++ (nucleotide (n `mod` 4))) (n `div` 4) k
          where nucleotide c = case c of {0 -> "A"; 1 -> "C"; 2-> "G"; 3 -> "T"}

patternCount :: String -> String -> Int
patternCount text pattern = patternCount' 0 text pattern
  where patternCount' :: Int -> String -> String -> Int
        patternCount' c [] pattern = c
        patternCount' c text pattern
          | take (length pattern) text == pattern = patternCount' (c+1) (drop 1 text) pattern
          | otherwise = patternCount' c (drop 1 text) pattern

mapMaxElements :: Map.Map String Int -> (Int, [String])
mapMaxElements m = (maxVal, map fst (filter isBiggest sorted))
  where sorted = reverse . sortBy(compare `on` snd) $ (Map.toList m)
        maxVal = snd $ head sorted
        isBiggest (k,v) = v == maxVal

mostFrequentKmers :: String -> Int -> (Int, [String])
mostFrequentKmers text k = mostFrequentKmers' text Map.empty
  where mostFrequentKmers' [] m = mapMaxElements m
        mostFrequentKmers' text m
          | length text < k = mostFrequentKmers' [] m
          | otherwise = mostFrequentKmers' (drop 1 text) (Map.insert kmer val m)
          where
            kmer = take k text
            val = Data.Maybe.fromMaybe 0 (Map.lookup kmer m) + 1

reverseComplement :: String -> String
reverseComplement dna = reverse $ map (\n -> case n of {'A' -> 'T'; 'T' -> 'A'; 'G' -> 'C'; 'C' -> 'G'}) dna

patternOccurences :: String -> String -> [Int]
patternOccurences text pattern = patternOccurences' text [] 0
  where
    patternOccurences' [] list index = reverse list
    patternOccurences' text list index
      | take (length pattern) text == pattern = patternOccurences' (drop 1 text) (index : list) (index+1)
      | otherwise = patternOccurences' (drop 1 text) list (index+1)
