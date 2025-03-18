myMaximum :: (Ord a) => [a] -> a
myMaximum [] = error "non value" -- minBound
myMaximum [x] = x
myMaximum (x : xs) = max x $ myMaximum xs

myReplicate :: (Ord t1, Num t1) => t1 -> t2 -> [t2]
myReplicate n x
  | n <= 0 = []
  | otherwise = x : myReplicate (n - 1) x

myTake :: (Ord t, Num t) => t -> [a] -> [a]
myTake _ [] = []
myTake n (x : xs)
  | n <= 0 = []
  | otherwise = x : myTake (n - 1) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

myRepeat :: t -> [t]
myRepeat x = x : myRepeat x

myQuickSort :: (Ord a) => [a] -> [a]
myQuickSort [] = []
myQuickSort (x : xs) =
  myQuickSort small ++ [x] ++ myQuickSort large
  where
    small = [v | v <- xs, v < x]
    large = [v | v <- xs, v >= x]
