-- Homework Assignment 1
-- Mohammad Ali


f :: Int
f = 2^63 - 1



-- Exercise 2
a, d :: Integer
b , c:: Double
a = 3
b = 99.3
c = b / fromIntegral (a)
d = 8

-- Ans: You can not have multiple declartions
-- of c, which means that 8 should be assigned
-- to some other letter which should also be an
-- int.
-- Also, 99.3 is not an int. So it has to be a
-- Double.
-- Since the answer of b/a is a double, C would
-- also be a double.
-- And we would have to convert with fromIntegral
-- to perform the division. The changes are shown
-- above in the code.

ints :: [Integer]
ints = 3 : 4 : [5,6]


ints1 = [(3,4,5,6,"seven")]
-- As we know that lists can only contain elements
-- of one type we would have to make a tuple or
-- our own data type to represent 3,4,5,6,"seven".

bar :: String
bar = "yx"

-- If we want to represent xy, we need a string as
-- a char can not represent two characters. The
-- correction is shown in the string above.

--Exercise 3

getrun :: [Integer] -> ([Integer], [Integer])
getrun [] = ([], [])
getrun (x:xs) = (getrunhelper x xs, getrunhelper2 x xs)

getrunhelper :: Integer -> [Integer] -> [Integer]
getrunhelper n [] = [n]
getrunhelper n (x:xs)
                | n == x = n : getrunhelper n xs
                -- Just return the first one if no repeating element.
                |otherwise = [n]

getrunhelper2 :: Integer -> [Integer] -> [Integer]
getrunhelper2 n [] = []
getrunhelper2 n (x:xs)
                  -- Jump over if same
                 |n == x = getrunhelper2 n xs
                 |otherwise = x:xs


--Exercise 4
lookandsay :: [Integer] -> [Integer]
lookandsay [] = []
lookandsay [0] = [1]
-- Got help from Bosco on how to access
-- two different outputs of a function.

lookandsay (x:xs) = case getrun (x:xs) of
  --to convert from Int to an Integer you call from Integral
  (l1, l2) -> (fromIntegral (length l1)) : x : lookandsay l2

--Exercise 5
lookandsaySeq' :: Integer -> [[Integer]]
lookandsaySeq' 0 = []
lookandsaySeq' n = lookandsayHelper n [0] : (lookandsaySeq' (n - 1))

-- Reversing the soloution from lookandsaySeq to
-- put it into the right order.
lookandsaySeq :: Integer -> [[Integer]]
lookandsaySeq n = reverse $ lookandsaySeq' n
--Caling look and say n times
lookandsayHelper :: Integer -> [Integer] -> [Integer]
-- if the thing is zero then we give whatever we are looking at
lookandsayHelper 0 l = l
lookandsayHelper n l = lookandsay (lookandsayHelper (n - 1) l)

-- Another way to do question 5.

lookandsaySeqHelp :: Integer -> [Integer]
lookandsaySeqHelp 0 = [0]
lookandsaySeqHelp n = lookandsay (lookandsaySeqHelp (n-1))

lookandsaySeq2' :: Integer -> [[Integer]]
lookandsaySeq2' 1 = [lookandsay [0]]
lookandsaySeq2' n = lookandsaySeqHelp n : lookandsaySeq2' (n -1)

lookandsaySeq2 :: Integer -> [[Integer]]
lookandsaySeq2 n = reverse (lookandsaySeq2' n)


-- Exercise 6

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c= [(a,c)]
-- We move all the disks over to b(Consider that as the destination)
-- until we reach the last disk. So we use "a c b" for the 1st hanoi call.
-- which is when we hit the middle and do the a => c move.
-- Then we move from peg b wihch should be considered a
-- to our final dest c. so We do "b a c" for our secong Hanoi
-- function.
hanoi n a b c = hanoi (n-1) a c b  ++ [(a , c)] ++ hanoi (n-1) b a c


-- Exercise 7
-- http://service.scs.carleton.ca/sites/default/files/tr/TR-04-10.pdf
-- Got help from this link
hanoi2 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi2 n a b c d = [(a,b)] ++ hanoi (n-1) a c  d ++ [(b,d)]

-- Exercise 8:
maxRegions :: Integer -> Integer
maxRegions 1 = 2
maxRegions n = n + maxRegions(n-1)
-- If we have no chords, we only have one region.
-- If we have 1 chord we get two regions. Similarly,
-- 3 chords give us 7 regions and so on. What we see
-- here is that the maximum number of regions formed,
-- by the n number of chords is the sum of number of
-- chords and the maximum number of regions formed by
-- n - 1 chords. Also we can see that in order to get
-- the maximum number of regions, we have to make sure
-- that the chords intersect and are not parallel.
