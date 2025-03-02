doubleMe :: (Num a) => a -> a
doubleMe x = x + x

doubleUs :: (Num a) => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

-- 引数を取らない関数
o'reilly :: String
o'reilly = "O'reilly"

-- 階乗
factorial :: (Num a, Enum a) => Int -> a
factorial n = product $ take n [1 ..]
