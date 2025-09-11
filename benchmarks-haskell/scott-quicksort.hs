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

scottFilter :: (Int -> Bool) -> ScottList Int -> ScottList Int
scottFilter f list =
  (unlist list)
    scottNil
    ( \x xs ->
        if f x
          then scottCons x (scottFilter f xs)
          else scottFilter f xs
    )

scottAppend :: ScottList Int -> ScottList Int -> ScottList Int
scottAppend xs ys = (unlist xs) ys (\x xss -> scottCons x (scottAppend xss ys))

scottQuicksort :: ScottList Int -> ScottList Int
scottQuicksort list =
  (unlist list)
    scottNil
    ( \x xs ->
        scottAppend
          (scottQuicksort (scottFilter (\y -> y < x) xs))
          (scottCons x (scottQuicksort (scottFilter (\z -> z >= x) xs)))
    )

scottSumList :: ScottList Int -> Int
scottSumList list = (unlist list) 0 (\x xs -> x + scottSumList xs)

generateList :: Int -> ScottList Int
generateList n = go 0 scottNil
  where
    go i acc
      | i < n = go (i + 1) (scottCons i acc)
      | otherwise = acc

main :: IO ()
main =
  print $ scottSumList $ scottQuicksort $ generateList 1000
