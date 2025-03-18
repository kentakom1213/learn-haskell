import qualified Data.List as L

numUnique :: (Eq a) => [a] -> Int
numUnique = length . L.nub


