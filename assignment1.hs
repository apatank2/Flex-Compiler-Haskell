remove :: Int -> [a] -> [a]
remove _ [] = []
remove n xs@(_:xs1)
   | n > 0     = remove (n-1) xs1
   | otherwise = xs
keepl n xs =  if n <= 0 then [] 
              else 
              case remove (n-1) xs of 
			  (z:zs) -> z : keepl n zs 
			  [] -> []
osublist [] = [[]]
osublist y = 
            getoddlist (sort_list y)
sort_list [] = [[]]
sort_list (x:xs) =  sort_list xs ++ [x:zs | zs <- sort_list xs]
getoddlist [] = []
getoddlist (y:ys)  | length(y) `mod` 2 == 1 = y:getoddlist ys 
                   | otherwise = getoddlist ys