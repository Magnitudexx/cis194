type Peg = String
type Move = (Peg, Peg)

{-
 Tower of Hanoi. 
 n: # of disks
 a: Peg where all disks start
 b: Peg destination
 c: Auxillary Peg
 -}
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c 
    | n <= 0        = []
    | otherwise     = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a
