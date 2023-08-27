-- you may change this to your own data type
newtype Set a = Set { unSet :: [a] } deriving (Show)


insertedsortedlist :: (Ord a) => a -> [a] -> [a]
insertedsortedlist x [] = [x]
insertedsortedlist x (y:ys) = if x <= y then x:y:ys
                     else y: insertedsortedlist x ys

removeDups :: Ord a => [a] -> [a]
removeDups [] = []
removeDups (x:xs) = x : (removeDups(remove x xs))
         where remove x [] = []
               remove x (y:ys) 
                  | x == y = remove x ys
                  | otherwise = y: (remove x ys)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [a | a <- xs, a <= x]
   ++ [x] ++
   qsort [b | b <- xs, b > x]

-- toList {2,1,4,3} => [1,2,3,4]
-- the output must be sorted.
toList :: Set a -> [a]
toList cs = (unSet cs)

-- fromList [2,1,1,4,5] => {2,1,4,5}
fromList :: Ord a => [a] -> Set a
fromList xs = Set(qsort(removeDups xs))


-- test if two sets have the same elements.
instance (Ord a) => Eq (Set a) where
  s1 == s2 = (toList s1 == toList s2)


-- the empty set
empty :: Set a
empty = Set([])


-- Set with one element
singleton :: a -> Set a
singleton x = Set([x])


-- insert an element of type a into a Set
-- make sure there are no duplicates!
insert :: (Ord a) => a -> Set a -> Set a
insert x cs = Set(insertedsortedlist x (unSet cs))


-- join two Sets together
-- be careful not to introduce duplicates.
union :: (Ord a) => Set a -> Set a -> Set a
union xs cs = Set(removeDups((unSet xs) ++ (unSet cs)))


-- return the common elements between two Sets
removeItem :: (Ord a) => a -> [a] -> [a]
removeItem _ [] = []
removeItem x (y:ys) | x == y = removeItem x ys
                    | otherwise = y : removeItem x ys

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection xs cs = Set(common (unSet xs) (unSet cs))
            where common :: (Ord a) => [a] -> [a] -> [a]
                  common _ [] = []
                  common [] _ = []
                  common (x:xss) css | x `elem` css = x : common xss (removeItem x css)   --if the item x is in css then remove x from css
                                     | otherwise = common xss css                         


-- all the elements in Set A *not* in Set B,
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a
difference xs cs = Set(uncommon (unSet xs) (unSet cs))
            where uncommon :: (Ord a) => [a] -> [a] -> [a]
                  uncommon _ [] = []
                  uncommon [] _ = []
                  uncommon (x:xss) css | x `notElem` css = x : uncommon xss (removeItem x css)  
                                       | otherwise = uncommon xss css                           --if x is present in css, then dont add it to the final output


-- is element *a* in the Set?
member :: (Ord a) => a -> Set a -> Bool
member x cs | x `elem` (unSet cs) = True
            | otherwise = False


-- how many elements are there in the Set?
cardinality :: Set a -> Int
cardinality cs = length(unSet cs)


setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap f xs = Set([f x | x <- (unSet xs)])

setfoldr :: (a -> b -> b) -> Set a -> b -> b
setfoldr f xs v = foldr f v (unSet xs) 


-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }

addhead :: a -> [a] -> [a]                                                             --adds the head to the end of the list
addhead c xs = xs ++ [c]

powerSetFirst :: [a] -> [[a]]                                                          --calculates the powerset in terms of a list
powerSetFirst [] = [[]]
powerSetFirst (x:xs) = [x : y | y <- powerSetFirst xs] ++ powerSetFirst xs

powerSetSecond2Set :: [[a]] -> [Set a] -> Set(Set a)                                   --converts the powerset to a set
powerSetSecond2Set [] z = Set z
powerSetSecond2Set (x:xs) z = powerSetSecond2Set xs (addhead (Set x) z)

powerSet :: Set a -> Set (Set a)                                                       --final powerset output
powerSet cs = (powerSetSecond2Set(powerSetFirst(toList cs)) [])


-- cartesian product of two sets
cartesian :: Set a -> Set b -> Set (a, b)
cartesian xs cs = Set([(x,c) | x <- (unSet xs), c <- (unSet cs)])


-- partition the set into two sets, with
-- all elements that satisfy the predicate on the left,
-- and the rest on the right
partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition p xs = (Set([x | x <- (unSet xs), (p x == True)]), Set([x | x <- (unSet xs), (p x == False)])) 

