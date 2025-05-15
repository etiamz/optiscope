module Index : sig
  type t = private int

  val of_int : int -> t
end

module Level : sig
  type t = private int

  val of_int : int -> t

  val to_index : current:t -> t -> Index.t

  val up : by:int -> t -> t
end

type term = Lambda of term | Apply of term * term | Var of Index.t

val var : int -> term

type value = VClosure of value list * term | VNeutral of neutral

and neutral = NVar of Level.t | NApply of neutral * value

val vvar : Level.t -> value

val vapply : neutral * value -> value

val normalize : lvl:Level.t -> rho:value list -> term -> unit

val normalize_at : lvl:Level.t -> rho:value list -> term -> unit

val denote : rho:value list -> term -> value

val quote : lvl:Level.t -> value -> unit
