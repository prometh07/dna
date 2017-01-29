patternToNumber :: String -> Integer
patternToNumber [] = 0
patternToNumber s = 4 * patternToNumber (init s) + symbolToNumber (last s)
  where symbolToNumber c = case c of {'A' -> 0; 'C' -> 1; 'G' -> 2; 'T' -> 3}

numberToPattern :: Int -> Int -> String
numberToPattern n k = pattern "" n k
  where pattern s 0 k = concat (replicate (k - length s) "A") ++ reverse s
        pattern s n k = pattern (s ++ (nucleotide (n `mod` 4))) (n `div` 4) k
          where nucleotide c = case c of {0 -> "A"; 1 -> "C"; 2-> "G"; 3 -> "T"}
