lengthOfList :: [t] -> Integer

lengthOfList (_:xs) = 1 + lengthOfList xs
lengthOfList [] = 0


meanOfList xs = (sum xs) / (fromIntegral (length xs))
