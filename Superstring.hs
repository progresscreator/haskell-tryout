-- David House
-- CS 320 PSet 2
-- 9/24/08

module Superstring  where
 import List

-- Problem 1
 type Str a = [a]

 -- A
 overlap :: Eq a => (Str a, Str a) -> Int
 overlap ((s1),(s2)) = if (isPrefixOf s1 s2) then length s1
                      else overlap ( (tail s1),(s2) )
 overlap (_,_) = 0

-- B
 contains ::  Eq a => Str a -> Str a -> Bool
 contains _ [] = False
 contains s1s s2s = any (containsHelp s2s) (tails s1s)

 containsHelp :: Eq a => Str a -> Str a -> Bool
 containsHelp [] _ = True
 containsHelp _ [] = False
 containsHelp [s] [v] = (s==v)
 containsHelp (s1:s1s) (s2:s2s) = (s1==s2) && containsHelp s1s s2s

-- C
 o :: Eq a => Str a -> Str a -> Str a
 s1 `o` s2 = s1 ++ (drop (overlap(s1,s2)) s2)

-- D
 naive :: Eq a => [Str a] -> Str a
 naive strs  = foldr (o) [] strs

-- Problem 2
 -- A
 maximize :: Eq a => (a -> Int) -> a -> a -> a
 maximize f s1 s2 = if (f s1) > (f s2) then s1
                    else s2

 -- B
 minimize :: Eq a => (a -> Int) -> a -> a -> a
 minimize f s1 s2 = if (f s1) < (f s2) then s1
                    else s2

-- Problem 3
 -- A
 update :: Eq a => [Str a] -> (Str a, Str a) -> [Str a]
 update l (s1,s2) = (s1 `o` s2) : filter (notContains (s1 `o` s2) ) l

 notContains ::  Eq a => Str a -> Str a -> Bool
 notContains s1 s2 = not( contains s1 s2)

 -- B
 allPairs :: Eq a => [Str a] -> [(Str a, Str a)]
 allPairs l  = makePair l l
 
 makePair :: Eq a => [Str a] -> [Str a] -> [(Str a, Str a)]
 makePair [] _ = []
 makePair l [] = makePair (tail l) (tail l)
 makePair l1 l2 = if not ((head l1) == (head l2))
                  then (head l1, head l2) :(head l2, head l1) :
                       makePair l1 (tail l2)
                  else makePair l1 (tail l2)

 -- C
 superstring :: Eq a => ([Str a] -> [(Str a,Str a)]) -> [Str a] -> Str a
 superstring next [] = []
 superstring next [a] = a
 superstring next l = foldr ( minimize length) (naive l) (map (superstring next) lol)
     where lol = [update l (x,x') | (x, x') <- next l]

 -- D
 optimal :: Eq a => [Str a] -> Str a
 optimal strs = superstring allPairs strs

 -- Problem 4
 -- A
 firstPair :: Eq a => [Str a] -> [(Str a,Str a)]
 firstPair strs = [((head strs), (head( tail strs)))]

 -- B
 bestWithFirst :: Eq a  => [Str a] -> [(Str a,Str a)]
 bestWithFirst strs  = [foldr (maximize overlap) ([], [])  pairs]             
     where  
     x = head strs
     pairs = [(x, y) | y <- (tail strs)]

 -- C
 bestPair :: Eq a  => [Str a] -> [(Str a,Str a)]
 bestPair strs  = [foldr (maximize overlap) ([], []) ( allPairs strs)]

 -- D
 greedy :: Eq a => [Str a] -> Str a
 greedy strs = foldr (minimize length) (naive strs) ((superstring firstPair strs):(superstring bestWithFirst strs):(superstring bestPair strs):[])

-- Problem 5
 -- A
 compare :: Eq a => ([Str a] -> Str a) -> ([Str a] -> Str a) -> [Str a] -> Double
 compare f1 f2 l = (fromIntegral (length (f1 l))) / (fromIntegral (length (f2 l )))

 -- B
 -- Greedy runs very quickly, whereas optimal takes a very long time to compute. -- firsPair gives a very long string, bestWithFirst gives a slightly shorter st -- ring, and bestPair gives the shortest.



