fizzBuzz :: (Integral a, Show a) => a -> String
fizzBuzz x =
  if x `mod` 15 == 0
    then "FizzBuzz"
    else
      if x `mod` 3 == 0
        then "Fizz"
        else
          if x `mod` 5 == 0
            then "Buzz"
            else show x

fizzBuzzN :: (Integral a, Show a) => a -> [String]
fizzBuzzN n = [fizzBuzz x | x <- [1 .. n]]
