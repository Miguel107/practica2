import Data.Char (digitToInt, isDigit)


isValidISBN :: String -> Bool
isValidISBN isbn = 
    let cleanedISBN = filter (\c -> isDigit c || c == 'X') isbn  
        isbnDigits = map (\c -> if c == 'X' then 10 else digitToInt c) cleanedISBN
        
        checkDigit = last isbnDigits 
        multipliers = reverse [1..10] 
        products = zipWith (*) isbnDigits multipliers 
        
        totalSum = sum products 
    in totalSum `mod` 11 == 0 

main :: IO ()
main = do
    putStrLn "Ingresa el código ISBN (con guiones si los tiene):"
    isbnInput <- getLine
    let result = isValidISBN isbnInput
    putStrLn $ "¿Es válido el ISBN ingresado? " ++ show result
