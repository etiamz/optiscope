import Interpreter

scottNil :: Term
scottNil = Lambda (Lambda (TVar 1))

scottCons :: Term
scottCons =
  Lambda (Lambda (Lambda (Lambda (Apply (Apply (TVar 0) (TVar 3)) (TVar 2)))))

scottSingleton :: Term
scottSingleton = Lambda (Apply (Apply scottCons (TVar 0)) scottNil)

lessEqual :: Term
lessEqual = BinaryOp (\x y -> if x <= y then 1 else 0)

scottInsert :: Term
scottInsert =
  Fix
    ( Lambda
        ( Lambda
            ( Lambda
                ( Apply
                    (Apply (TVar 0) (Apply scottSingleton (TVar 1)))
                    ( Lambda
                        ( Lambda
                            ( IfThenElse
                                (Apply (Apply lessEqual (TVar 3)) (TVar 1))
                                ( Apply
                                    (Apply scottCons (TVar 3))
                                    (Apply (Apply scottCons (TVar 1)) (TVar 0))
                                )
                                ( Apply
                                    (Apply scottCons (TVar 1))
                                    (Apply (Apply (TVar 4) (TVar 3)) (TVar 0))
                                )
                            )
                        )
                    )
                )
            )
        )
    )

scottInsertionSort :: Term
scottInsertionSort =
  Fix
    ( Lambda
        ( Lambda
            ( Apply
                (Apply (TVar 0) scottNil)
                (Lambda (Lambda (Apply (Apply scottInsert (TVar 1)) (Apply (TVar 3) (TVar 0)))))
            )
        )
    )

sumInts :: Term
sumInts = BinaryOp (+)

scottSumList :: Term
scottSumList =
  Fix
    ( Lambda
        ( Lambda
            ( Apply
                (Apply (TVar 0) (Const 0))
                (Lambda (Lambda (Apply (Apply sumInts (TVar 1)) (Apply (TVar 3) (TVar 0)))))
            )
        )
    )

generateList :: Int -> Term
generateList n = go 0 scottNil
  where
    go i acc
      | i < n = go (i + 1) (Apply (Apply scottCons (Const i)) acc)
      | otherwise = acc

benchmarkTerm :: Term
benchmarkTerm =
  Apply scottSumList (Apply scottInsertionSort (generateList 5000))

main :: IO ()
main = case denote [] benchmarkTerm of
  VConst n -> print n
  _ -> error "Expected a constant result!"
