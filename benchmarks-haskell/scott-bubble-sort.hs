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

scottBubbleSwap :: ScottList Int -> Int -> ScottList Int
scottBubbleSwap list n
  | n == 0 = list
  | otherwise =
      (unlist list)
        list
        ( \x xs ->
            (unlist xs)
              list
              ( \y ys ->
                  if x < y
                    then scottCons x (scottBubbleSwap xs (n - 1))
                    else scottCons y (scottBubbleSwap (scottCons x ys) (n - 1))
              )
        )

scottBubbleGo :: ScottList Int -> Int -> ScottList Int
scottBubbleGo list n
  | n == 0 = list
  | otherwise = scottBubbleGo (scottBubbleSwap list n) (n - 1)

scottListLength :: ScottList Int -> Int
scottListLength list = (unlist list) 0 (\x xs -> 1 + scottListLength xs)

scottBubbleSort :: ScottList Int -> ScottList Int
scottBubbleSort list =
  (unlist list)
    scottNil
    ( \x xs ->
        scottBubbleGo list (scottListLength list - 1)
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
  print $ scottSumList $ scottBubbleSort $ generateList 1000
