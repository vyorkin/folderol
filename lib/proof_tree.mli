(** A proof tree records the structure of a completed proof. *)

type t =
  | Axiom of Goal.t * Formula.t
      (** A leaf node: the goal was solved by unifying [formula]. *)
  | Step of Goal.t * Rule.t * t list
      (** An internal node: [rule] was applied to [goal], producing subtrees. *)
[@@deriving show { with_path = false }]

val equal : t -> t -> bool

val goal : t -> Goal.t
(** Returns the goal at the root of the proof tree. *)

val pp : Format.formatter -> t -> unit
val to_string : t -> string
