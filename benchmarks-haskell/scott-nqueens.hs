{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Redundant bracket" #-}

newtype ScottList a = ScottList (forall r. r -> (a -> ScottList a -> r) -> r)

unlist :: ScottList a -> forall r. r -> (a -> ScottList a -> r) -> r
unlist (ScottList f) = f

scottNil :: ScottList a
scottNil = ScottList (\n c -> n)

scottCons :: a -> ScottList a -> ScottList a
scottCons h t = ScottList (\n c -> c h t)

scottMember :: Int -> ScottList Int -> Bool
scottMember x list = (unlist list) False (\y ys -> x == y || scottMember x ys)

scottLength :: ScottList a -> Int
scottLength list = (unlist list) 0 (\x xs -> 1 + scottLength xs)

scottAppend :: ScottList a -> ScottList a -> ScottList a
scottAppend xs ys = (unlist xs) ys (\x xss -> scottCons x (scottAppend xss ys))

scottThreat :: Int -> Int -> ScottList Int -> Bool
scottThreat k m list =
  (unlist list)
    False
    (\x xs -> k == x - m || k == m - x || scottThreat (k + 1) m xs)

scottQueenAux :: Int -> ScottList Int -> Int -> ScottList (ScottList Int)
scottQueenAux 0 b n = scottNil
scottQueenAux m b n
  | scottMember m b || scottThreat 1 m b =
      scottQueenAux (m - 1) b n
  | scottLength b == n - 1 =
      scottAppend (scottCons (scottCons m b) scottNil) (scottQueenAux (m - 1) b n)
  | otherwise =
      scottAppend (scottQueenAux n (scottCons m b) n) (scottQueenAux (m - 1) b n)

scottQueen :: Int -> ScottList (ScottList Int)
scottQueen n = scottQueenAux n scottNil n

main :: IO ()
main = print $ scottLength $ scottQueen 12
