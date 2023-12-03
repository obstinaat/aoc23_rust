main :: IO ()

calc :: [Char] -> Int
calc "" = 0
calc (x:content) 
    | x == '(' = (+1) (calc content)
    | x == ')' = subtract 1 (calc content)

main = do 
    content <- readFile "input.txt"
    print(calc content)