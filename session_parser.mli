type session = Session of Filter.Base.session | Destruction of string * Date.t

val parse_xensource_session : Date.t -> string -> session option
