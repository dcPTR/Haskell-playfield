import Control.Monad

rotate [] = []
rotate (x:xs) = xs ++ [x]
rotations n = take (length n) $ iterate rotate n

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concatMap (rotations.(x:)) (perms xs)

listToInteger list = foldl ((+).(*10)) 0 list

isDivisable :: Integer -> Integer -> Integer
isDivisable num k = do
  if (num `mod` k == 0) then 1
  else 0

check :: [Integer] -> Integer -> Integer
check list k = do
  if (list !! 0 == 0) then 0
  else isDivisable (listToInteger list) k

countPandigitals p k = foldl(\acc singleP -> acc+(check singleP k)) 0 p

main :: IO ()
main = do
    let n = 2
    let input = [0,0,1,1,2,2,3,3]
    let p = perms input
    let c = countPandigitals p n
    let wynik = c `div` (2^((length input) `div` 2))

    print(wynik)