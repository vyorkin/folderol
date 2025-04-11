type t [@@deriving eq, show]

val mk : unit -> t
val of_string : string -> t
