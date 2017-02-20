quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = (quickSort lower) ++ [splitter] ++ [quickSort upper]
    where
    splitter = x
    lower = [ y | y <-xs, y<=x ]
    upper = [ y | y <- xs, y>x ]
