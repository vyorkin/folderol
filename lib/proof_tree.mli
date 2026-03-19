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

val pp_ascii : Format.formatter -> t -> unit
(** Pretty-print the proof tree as an indented ASCII tree. *)

val to_string_ascii : t -> string
(** Format the proof tree as an indented ASCII string. *)

val to_latex : t -> string
(** Emit a LaTeX proof tree using the bussproofs package. *)
