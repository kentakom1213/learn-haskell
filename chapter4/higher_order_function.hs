myQuickSort :: (Ord a) => [a] -> [a]
myQuickSort [] = []
myQuickSort (x : xs) =
  myQuickSort small ++ [x] ++ myQuickSort large
  where
    small = filter (x >=) xs
    large = filter (x <) xs
