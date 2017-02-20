repl :: [Char] -> [Char]
repl [] = []
repl (x:xs) = x:x:(repl xs)
