main :: IO ()

countOpen :: [Char] -> Int
countOpen input = length (filter (isOpenParen) input)

countClose :: [Char] -> Int
countClose input = length (filter (isCloseParen) input)

isOpenParen :: Char -> Bool
isOpenParen c = c == '('

isCloseParen :: Char -> Bool
isCloseParen c = c == ')'

main = do 
    content <- readFile "input.txt"
    print(countOpen content - countClose content)