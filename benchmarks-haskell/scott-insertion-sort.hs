{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Redundant bracket" #-}

newtype ScottList a = ScottList (forall r. r -> (a -> ScottList a -> r) -> r)

unlist :: ScottList a -> forall r. r -> (a -> ScottList a -> r) -> r
unlist (ScottList f) = f

scottNil :: ScottList a
scottNil = ScottList (\n c -> n)

scottCons :: a -> ScottList a -> ScottList a
scottCons h t = ScottList (\n c -> c h t)

scottSingleton :: a -> ScottList a
scottSingleton x = scottCons x scottNil

scottInsert :: Int -> ScottList Int -> ScottList Int
scottInsert y list =
  (unlist list)
    (scottSingleton y)
    ( \z zs ->
        if y <= z
          then scottCons y (scottCons z zs)
          else scottCons z (scottInsert y zs)
    )

scottInsertionSort :: ScottList Int -> ScottList Int
scottInsertionSort list =
  (unlist list) scottNil (\x xs -> scottInsert x (scottInsertionSort xs))

scottSumList :: ScottList Int -> Int
scottSumList list =
  (unlist list) 0 (\x xs -> x + scottSumList xs)

generateList :: Int -> ScottList Int
generateList n = go 0 scottNil
  where
    go i acc
      | i < n = go (i + 1) (scottCons i acc)
      | otherwise = acc

main :: IO ()
main =
  print $ scottSumList $ scottInsertionSort $ generateList 1000
