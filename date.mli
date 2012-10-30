type date
type time
type t

val make_date : year:int -> month:int -> day:int -> date
val make_time : hours:int -> minutes:int -> seconds:int -> ms:int -> time
val make : year:int -> month:int -> day:int -> hours:int -> minutes:int -> seconds:int -> ms:int -> line:int -> t
val make2 : time:time -> date:date -> t

val zero : t
val max_date : t

val date : t -> date
val time : t -> time

val compare_date : date -> date -> int
val compare_time : time -> time -> int
val compare : t -> t -> int

val sort : t -> t -> t * t
val min : t -> t -> t
val max : t -> t -> t

val to_float : t -> float

module Duration :
sig
	type t
	val zero : t
	val max_duration : t
	val max : t -> t -> t
	val min : t -> t -> t
	val compare : t -> t -> int
	val of_string : string -> t
	val to_string : ?verbose:bool -> t -> string
	val to_float : t -> float
end

val sub : t -> t -> Duration.t

module String_of :
sig
	val date : date -> string
	val time : ?verbose:bool -> time -> string
	val t : ?sep:string -> ?verbose:bool -> t -> string
end
