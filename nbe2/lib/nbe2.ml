module Index : sig
  type t = private int

  val of_int : int -> t
end = struct
  type t = int

  let of_int (idx : int) : t =
    assert (idx >= 0);
    idx
end

module Level : sig
  type t = private int

  val of_int : int -> t

  val to_index : current:t -> t -> Index.t

  val up : by:int -> t -> t
end = struct
  type t = int

  let of_int (lvl : int) : t =
    assert (lvl >= 0);
    lvl

  let to_index ~(current : t) self : Index.t = Index.of_int (current - self - 1)

  let up ~(by : int) self : t =
    assert (by >= 0);
    self + by
end

type term = Lambda of term | Apply of term * term | Var of Index.t

let var idx = Var (Index.of_int idx)

type value = VClosure of value list * term | VNeutral of neutral

and neutral = NVar of Level.t | NApply of neutral * value

let vvar lvl = VNeutral (NVar lvl)

let vapply (m, n) = VNeutral (NApply (m, n))

let rec normalize ~(lvl : Level.t) ~(rho : value list) : term -> unit =
 fun t -> quote ~lvl (denote ~rho t)

and normalize_at ~(lvl : Level.t) ~(rho : value list) : term -> unit =
 fun t -> normalize ~lvl:(Level.up ~by:1 lvl) ~rho:(vvar lvl :: rho) t

and denote ~(rho : value list) : term -> value = function
  | Lambda m -> VClosure (rho, m)
  | Apply (m, n) -> (
      let m_val = denote ~rho m in
      let n_val = denote ~rho n in
      match m_val with
      | VClosure (rho, m) -> denote ~rho:(n_val :: rho) m
      | VNeutral neutral -> vapply (neutral, n_val))
  | Var idx -> List.nth rho (idx :> int)

and quote ~(lvl : Level.t) : value -> unit = function
  | VClosure (rho, m) ->
      Sys.opaque_identity (normalize_at ~lvl ~rho m);
      ()
  | VNeutral neutral -> quote_neutral ~lvl neutral

and quote_neutral ~(lvl : Level.t) : neutral -> unit = function
  | NVar var ->
      ignore (Sys.opaque_identity (Var (Level.to_index ~current:lvl var)));
      ()
  | NApply (neut, n_val) ->
      Sys.opaque_identity (quote_neutral ~lvl neut);
      Sys.opaque_identity (quote ~lvl n_val);
      ()
