type session = Session of Filter.Base.session | Destruction of string * Date.t

val parse_xensource : ?log_counter:int -> string -> 
  ((Date.t * string * Log.level * string option * int * string option * string option * string * Log.error * string) option * session option)










