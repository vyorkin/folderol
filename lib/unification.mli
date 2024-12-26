module Env : sig
  type t

  val empty : t
  val add : t -> string * Term.t -> t
  val find : t -> string -> Term.t option
end

val unify : Env.t -> Formula.t * Formula.t -> (Env.t, string) result
