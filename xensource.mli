val parse_xensource : ?log_counter:int -> string -> 
  (Date.t * string * Log.level * string option * int * string option * string option * string * string) option

val cache_task_names : (string, string) Hashtbl.t
