import Control.Monad

rotate [] = []
rotate (x:xs) = xs ++ [x]
rotations n = take (length n) $ iterate rotate n

isPrime k = if k > 1 then null [ x | x <- [2..k-1], k `mod` x == 0] else False

isCircularPrime :: Integer -> Bool
isCircularPrime x = do
  let rotList = map (read :: String->Integer) $ rotations $ show x
  let wynik = filter isPrime rotList
  
  if (length wynik == length rotList) then True
  else False

main :: IO ()
main = do 
    let n = 10000
    let w = filter isCircularPrime [2..n]
    print(w)