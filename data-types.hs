type ISBN = Int
type Name = String
type AuthorName = Name

data Book = Book ISBN Name [AuthorName]
            deriving (Show)
-- That was the basic


-- Enumerations
data Contact = InternalContact
             | ExternalContact
             | FirmContact
             deriving (Show)

-- Custom constructors
type Point = (Double, Double)

data Shape = Circle Point Double
           | Square Point Double
           | Rectangle Point Double Double
           deriving (Eq,Show)

sq = Square (0,0) 10
rec = Rectangle (10,2) 10 100


area (Square _ side) = side * side
area (Rectangle _ height width) = height * width
area (Circle _ radius) = pi * (radius ^ 2)


sumOfList (x:xs) = x + sumOfList xs
sumOfList [] = 0


bookName (Book _ name _) = name


-- Record syntax
data Customer = Customer {
                  customerID :: Int,
                  customerName :: String,
                  customerAddress :: [String]
                  } deriving (Show)


-- Parameterized types
data Optional t = Ojust t
                | Onothing


-- Recursive types
data List t = Cons t (List t)
            | Nil
            deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

toList (Cons x xs) = x:(toList xs)
toList Nil = []

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

data TreeN a = NodeN a (Maybe (TreeN a)) (Maybe (TreeN a))
               deriving (Show)


unsafeSecond xs = if null (tail xs)
                  then error "list is too short"
                  else head (tail xs)

safeSecond (_:x:_) = Just x
safeSecond _ = Nothing


-- Variables defined in let block and and used after in
lend amount balance = let reserve = 100
                          newBalance = balance - amount
                      in if balance < reserve
                         then Nothing
                         else if amount >= balance
                         then Nothing
                         else Just newBalance

lend2 amount balance = if balance < reserve
                       then Nothing
                       else if amount >= balance
                       then Nothing
                       else Just newBalance
    where reserve    = 100
          newBalance = balance - amount


-- Defining functions in where/let
pluralize word counts = map plural counts
       where plural 0 = "no " ++ word ++ "s"
             plural 1 = "one " ++ word
             plural n = show n ++ " " ++ word ++ "s"


fromMaybe defaultVal wrapped =
    case wrapped of
        Nothing -> defaultVal
        Just val -> val


-- Guards
nodesAreSame (Node a _ _) (Node b _ _)
    | a == b = True
nodesAreSame _ _ = False


lendWithGuards amount balance
    | balance < reserve = Nothing
    | amount >= balance = Nothing
    | otherwise = Just newBalance
  where reserve     = 100
        newBalance  = balance - amount

-- otherwise guard basically is True

