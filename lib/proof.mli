val init : Goal_table.t -> unit
val clear : unit -> unit
val step : unit -> (Goal_table.t, string) result
val steps : int -> (Goal_table.t, string) result
val print_goal_table : unit -> unit
