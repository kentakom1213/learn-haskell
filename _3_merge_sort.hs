-- マージソートの実装
myMergeSort :: (Ord a1) => [a1] -> [a1]
myMergeSort [] = []
myMergeSort [x] = [x]
myMergeSort ls =
  myMerge (myMergeSort left) (myMergeSort right)
  where
    len = length ls
    mid = len `div` 2
    left = take mid ls
    right = drop mid ls

myMerge :: (Ord a) => [a] -> [a] -> [a]
myMerge left [] = left
myMerge [] right = right
myMerge left@(x : xs) right@(y : ys)
  | x <= y = x : myMerge xs right
  | otherwise = y : myMerge left ys
