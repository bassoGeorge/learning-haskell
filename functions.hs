add a b = a + b

myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)

-- We can  explicitly add type signatures
niceDrop :: Integer -> [Integer] -> [Integer]
niceDrop n xs | n <= 0 = xs
niceDrop _ []          = []
niceDrop n (_:xs)      = niceDrop (n - 1) xs

-- or
niceDrop2 n xs = case (n, xs) of
                      (n, xs) | n <= 0 -> xs
                      (_, [])          -> []
                      (n, (_:xs))      -> niceDrop2 (n - 1) xs



lastButOne xs = if null (tail (tail xs))
                then head xs
                else lastButOne (tail xs)


-- Defining data types
data BookInfo = Book Int String [String]
                deriving (Show)

-- type <synonym> = <original type>
type ISBN = Int

-- The data <TypeContructor>  = <ValueConstructor> [components]...
-- It is normal for TypeConstructor to be same as ValueConstructor
data MagazineInfo = MagazineInfo ISBN String [String]
                    deriving (Show)
