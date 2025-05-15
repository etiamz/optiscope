open Nbe2

let church_add () =
  Lambda
    (Lambda
       (Lambda
          (Lambda
             (Apply (Apply (var 3, var 1), Apply (Apply (var 2, var 1), var 0))))))

let church_multiply () =
  Lambda
    (Lambda
       (Lambda (Lambda (Apply (Apply (var 3, Apply (var 2, var 1)), var 0)))))

let church_two () = Lambda (Lambda (Apply (var 1, Apply (var 1, var 0))))

let church_five () =
  Lambda
    (Lambda
       (Apply
          ( var 1,
            Apply (var 1, Apply (var 1, Apply (var 1, Apply (var 1, var 0)))) )))

let church_ten () =
  Apply (Apply (church_multiply (), church_five ()), church_two ())

let church_20 () = Apply (Apply (church_add (), church_ten ()), church_ten ())

let church_25 () = Apply (Apply (church_add (), church_20 ()), church_five ())

let church_two_in_25 () = Apply (church_25 (), church_two ())

let () =
  ignore
    (Sys.opaque_identity
       (normalize ~lvl:(Level.of_int 0) ~rho:[] (church_two_in_25 ())))
