minSkew :: String -> [Int]
minSkew dna = minSkew' dna 1 0 (maxBound :: Int) []
  where
    minSkew' [] pos gc min_val list = reverse list
    minSkew' dna pos gc min_val list = minSkew' (tail dna) (pos+1) new_gc new_min new_list
      where new_gc = gc + case (head dna) of {'G'-> 1; 'C' -> (-1); 'T' -> 0; 'A' -> 0}
            new_min = if new_gc < min_val then new_gc else min_val
            new_list
              | new_gc > min_val = list
              | new_gc == min_val = pos : list
              | otherwise = [pos]

skew :: String -> [Int]
skew dna = skew' $ map (\n -> case n of {'A' -> 0; 'T' -> 0; 'G' -> 1; 'C' -> (-1)}) dna
  where skew' [] = []
        skew' [x] = [x]
        skew' (x:xs) = x : (skew' $ (head xs + x) : tail xs)
