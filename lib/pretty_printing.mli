val pp_comma : Format.formatter -> unit -> unit
(** Print a comma, typically used for separating elements in a list or similar
    structures. *)

val format_to_string : (Format.formatter -> 'a -> unit) -> 'a -> string
(** [format_to_string pp v] formats a value [v] using the provided
    pretty-printing function [pp], and returns the result as a string. This
    function handles buffer management and ensures that the formatted value is
    properly flushed to the string.

    - [pp] is a pretty-printing function that takes a [Format.formatter] and a
      value of type ['a] and prints it.
    - [v] is the value to be formatted.

    Returns the string representation of the formatted value. *)
