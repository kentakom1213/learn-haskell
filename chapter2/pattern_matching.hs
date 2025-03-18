import Text.XHtml (height)

-- {-# OPTIONS -Wall -Werror #-}

luckey :: (Eq a, Num a) => a -> String
luckey 7 = "LUCKEY NUMBER SEVEN"
luckey _ = "Sorry, you're out of luck, pal"

sayMe :: (Eq a, Num a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe _ = "Not Between 1 and 3"

-- 階乗（再帰実装）
factorial :: (Ord t, Num t) => t -> t
factorial 0 = 1
factorial n
  | n > 0 = n * factorial (n - 1)
  | otherwise = error "can't calculate"

first :: (a, b, c) -> a
first (a, _, _) = a

top :: (Show a) => [a] -> String
top (x : _) = "Top Item: " ++ show x
top _ = "Non Item"

firstLetter "" = "Empty string"
firstLetter all@(x : _) = "The first letter of " ++ show all ++ " is " ++ show x

bmiTell :: (Fractional a, Ord a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "痩せ気味"
  | bmi <= normal = "普通"
  | bmi <= fat = "太り気味"
  | otherwise = "クジラ"
  where
    bmi = weight / height ^ 2
    skinny = 18.5
    normal = 25.0
    fat = 30.0

describeList ls =
  "The list is "
    ++ case ls of
      [] -> "empty."
      [x] -> "a singleton list."
      xs -> "a longer list."
