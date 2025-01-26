(** Module representing the environment for unification.

    The environment is used to avoid repeatedly substituting meta-variables
    throughout the equations every time a meta-variable is assigned. Instead,
    it maintains a mapping of meta-variable -> term assignments and resolves
    terms on-the-fly.

    Keeping environment allows us to ensure that cycles are 
    prevented using occurs check which disallows assignments like [(?b, f(?a)), (?a, g(?b))]. *)

type t [@@deriving eq]
(** Type representing the environment,
      which maps meta-variables to terms. *)

val empty : t
(** Empty environment with no variable assignments. *)

val add : t -> string * Term.t -> t
(** Adds an assignment [var] -> [term] to the environment [env].

    @param env The existing environment.
    @param var The variable being assigned.
    @param term The term assigned to [var].
    @return The updated environment.
    @raise [Map.Key_already_present] if [var] is already assigned in [env]. *)

val find : t -> string -> Term.t option
(** Looks up the assignment for [var] in [env].

    @param env The environment to search.
    @param var The variable whose assignment is sought.
    @return [Some term] if [var] is assigned to [term], otherwise [None]. *)

val pp : Format.formatter -> t -> unit
(** Prints the env [t] using the format output function [fmt].

    @param fmt The formatter to which the formatted env is output.
    @param t The env to be formatted. *)

val equal : t -> t -> bool
(** Returns [true] if two environments are equal. *)
