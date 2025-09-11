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

scottSplit :: ScottList a -> (forall r. (ScottList a -> ScottList a -> r) -> r)
scottSplit list k =
  (unlist list)
    (k scottNil scottNil)
    ( \x xs ->
        (unlist xs)
          (k (scottSingleton x) scottNil)
          ( \y ys ->
              scottSplit
                ys
                (\left right -> k (scottCons x left) (scottCons y right))
          )
    )

scottMerge :: ScottList Int -> ScottList Int -> ScottList Int
scottMerge xs ys =
  (unlist ys)
    xs
    ( \y yss ->
        (unlist xs)
          ys
          ( \x xss ->
              if x < y
                then scottCons x (scottMerge xss ys)
                else scottCons y (scottMerge xs yss)
          )
    )

scottMergeSort :: ScottList Int -> ScottList Int
scottMergeSort list =
  (unlist list)
    scottNil
    ( \x xs ->
        (unlist xs)
          (scottSingleton x)
          ( \dummy dummyx ->
              scottSplit
                list
                ( \left right ->
                    scottMerge (scottMergeSort left) (scottMergeSort right)
                )
          )
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
  print $ scottSumList $ scottMergeSort $ generateList 1000
