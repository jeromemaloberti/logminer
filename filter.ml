(* Copyright (c) Citrix Systems 2008-2009. All rights reserved. *)
(* Author: Thomas Gazagnaire <thomas.gazagnaire@citrix.com>     *)

open Printf

type compare = LE | LEQ | GE | GEQ | EQ | NE

type compare_date =
	| Date of compare * Date.date
	| Time of compare * Date.time
	| Full_date of compare * Date.t

type task =
	| Task_ref of string
	| Task_uuid of string
	| Task_name of string
	| Task_root of t
	| Task_parents of t
	| Task_children of t
	| Task_lines of compare * int
	| Task_in_database
	| Task_duration of compare * Date.Duration.t
	| Task_nb_children of compare * int
	| Task_not_completed
	| Task_gap_duration of compare * Date.Duration.t

and session =
	| Session_trackid of string
	| Session_uname of string
	| Session_creation of compare_date
	| Session_destruction of compare_date
	| Session_duration of compare * Date.Duration.t
	| Session_nb_tasks of compare * int

and log =
	| Log_date of compare_date
	| Log_host of string
	| Log_thread_id of int
	| Log_thread_name of string
	| Log_error
	| Log_exception
	| Log_level of Log.level
	| Log_key of string
	| Log_gap_duration of compare * Date.Duration.t
	| Log_msg of string

and item =
	| Log of log
	| Task of task
	| Session of session

and t =
	| Not of t
	| All
	| Item of item
	| Or of t list
	| And of t list

module Base =
struct
	type filter = t
	type task = filter Task.t
	type log = task Log.t
	type session = task Session.t
end

module String_of =
struct
	
	module Name =
	struct
		let task = function
			| Task_ref _ -> "task-ref"
			| Task_uuid _ -> "task-uuid"
			| Task_name _ -> "task-name"
			| Task_root _ -> "task-root"
			| Task_parents _ -> "task-parent"
			| Task_children _ ->"task-children"
			| Task_lines _ -> "task-lines"
			| Task_in_database -> "task-in-database"
			| Task_duration _ -> "task-duration"
			| Task_nb_children _ -> "task-nb-children"
			| Task_not_completed -> "task-not-completed"
			| Task_gap_duration _ -> "task-gap-duration"
		
		let session = function
			| Session_trackid _ -> "session-trackid"
			| Session_uname _ -> "session-uname"
			| Session_creation _ -> "session-creation"
			| Session_destruction _ -> "session-destruction"
			| Session_duration _ -> "session-duration"
			| Session_nb_tasks _ -> "session-nb-tasks"
		
		let log = function
			| Log_date _ -> "date"
			| Log_host _ -> "host"
			| Log_thread_id _ -> "thread-id"
			| Log_thread_name _ -> "thread-name"
			| Log_error -> "error"
			| Log_exception -> "exception"
			| Log_level _ -> "level"
			| Log_key _ -> "key"
			| Log_gap_duration _ ->"gap-duration"
			| Log_msg _ -> "message"
		
	end
	
	let compare = function
		| LE -> "<"
		| LEQ -> "<="
		| GE -> ">"
		| GEQ -> ">="
		| EQ -> "="
		| NE -> "!="
	
	let compare_date = function
		| Date (c, d) -> sprintf "%s %s" (compare c) (Date.String_of.date d)
		| Time (c, d) -> sprintf "%s %s" (compare c) (Date.String_of.time d)
		| Full_date (c, d) -> sprintf "%s %s" (compare c) (Date.String_of.t d)
	
	let rec task f = match f with
		| Task_ref r -> sprintf "%s = %s" (Name.task f) r
		| Task_uuid u -> sprintf "%s = %s" (Name.task f) u
		| Task_name n -> sprintf "%s = %s" (Name.task f) n
		| Task_root x -> sprintf "%s(%s)" (Name.task f) (t x)
		| Task_parents x -> sprintf "%s(%s)" (Name.task f) (t x)
		| Task_children x -> sprintf "%s(%s)" (Name.task f) (t x)
		| Task_lines (c, i) -> sprintf "%s %s %i" (Name.task f) (compare c) i
		| Task_in_database -> sprintf "%s" (Name.task f)
		| Task_duration (c, d) -> sprintf "%s %s %s" (Name.task f) (compare c) (Date.Duration.to_string d)
		| Task_nb_children (c, i) -> sprintf "%s %s %i" (Name.task f) (compare c) i
		| Task_not_completed -> sprintf "%s" (Name.task f)
		| Task_gap_duration (c, d) -> sprintf "%s %s %s" (Name.task f) (compare c) (Date.Duration.to_string d)
	
	and session f = match f with
		| Session_trackid t -> sprintf "%s = %s" (Name.session f) t
		| Session_uname u -> sprintf "%s = %s" (Name.session f) u
		| Session_creation d -> sprintf "%s %s" (Name.session f) (compare_date d)
		| Session_destruction d -> sprintf "%s %s" (Name.session f) (compare_date d)
		| Session_duration(c, d) -> sprintf "%s %s %s" (Name.session f) (compare c) (Date.Duration.to_string d)
		| Session_nb_tasks (c, i) -> sprintf "%s %s %i" (Name.session f) (compare c) i
	
	and log f = match f with
		| Log_date d -> sprintf "%s %s" (Name.log f) (compare_date d)
		| Log_host h -> sprintf "%s = %s" (Name.log f) h
		| Log_thread_id i -> sprintf "%s = %i" (Name.log f) i
		| Log_thread_name n -> sprintf "%s = %s" (Name.log f) n
		| Log_error -> sprintf "%s" (Name.log f)
		| Log_exception -> sprintf "%s" (Name.log f)
		| Log_level l -> sprintf "%s = %s" (Name.log f) (Log.string_of_level l)
		| Log_key k -> sprintf "%s = %s" (Name.log f) k
		| Log_gap_duration(c, d) -> sprintf "%s %s %s" (Name.log f) (compare c) (Date.Duration.to_string d)
		| Log_msg m -> sprintf "%s = %s" (Name.log f) m
	
	and item = function
		| Log l -> log l
		| Task t -> task t
		| Session s -> session s
	
	and t = function
		| Not x -> sprintf "not(%s)" (t x)
		| All -> "all"
		| Item i -> item i
		| Or l -> String.concat " or " (List.map (fun x -> sprintf "(%s)" (t x)) l)
		| And l -> String.concat " and " (List.map (fun x -> sprintf "(%s)" (t x)) l)
end

module type Item =
sig
	type item
	val filter_task : t -> Base.task -> bool
	val to_logs : item -> Base.log list
	val to_tasks : item -> Base.task list
	val to_sessions : item -> Base.session list
end

module type Filter =
sig
	type item
	val create : t -> (item -> bool)
end

module Make (Item: Item) : Filter with type item = Item.item =
struct
	type item = Item.item
	
	let exists_task f item = List.exists f (Item.to_tasks item)
	let exists_log f item = List.exists f (Item.to_logs item)
	let exists_session f item = List.exists f (Item.to_sessions item)
	
	let compare : compare -> (int -> int -> bool) = function
		| LE -> (<)
		| LEQ -> (<=)
		| GE -> (>)
		| GEQ -> (>=)
		| EQ -> (=)
		| NE -> (<>)
	
	let compare_aux f comp a b = compare comp (f a b) 0
	
	let compare_duration = compare_aux Date.Duration.compare
	let compare_date = function
		| Date (c, d) -> (fun fd -> compare_aux Date.compare_date c (Date.date fd) d)
		| Time (c, d) -> (fun fd -> compare_aux Date.compare_time c (Date.time fd) d)
		| Full_date (c, d) -> (fun fd -> compare_aux Date.compare c fd d)
	
	let task filter =
		let fn = match filter with
			| Task_ref r -> (fun task -> Util.regexp_match r (Task.task_ref task))
			| Task_uuid u -> (fun task -> Util.regexp_match_option u (Task.task_uuid task))
			| Task_name n -> (fun task -> Util.regexp_match n (Task.task_name task))
			| Task_root t -> (fun task -> task.Task.parent = Task.No_parent && Task.exists_among_children (Item.filter_task t) task)
			| Task_parents t -> Task.exists_among_children (Item.filter_task t)
			| Task_children t -> Task.exists_among_parents (Item.filter_task t)
			| Task_in_database -> Task.is_in_db
			| Task_duration (c, d) -> (fun task -> compare_duration c (Task.duration task) d)
			| Task_lines (c, l) -> (fun task -> compare c (Task.get_lines task) l)
			| Task_nb_children (c, i) -> (fun task -> compare c (Task.get_nb_total_children task) i)
			| Task_not_completed -> Task.not_completed
			| Task_gap_duration (c, d) -> (fun task -> compare_duration c (Task.gap task) d)
		in
		exists_task fn
	
	let session filter =
		let fn = match filter with
			| Session_trackid t -> (fun session -> Session.trackid session = t)
			| Session_uname u -> (fun session -> Session.uname session = u)
			| Session_creation d -> (fun session -> compare_date d (Session.creation session))
			| Session_destruction d -> (fun session -> compare_date d (Session.destruction session))
			| Session_duration (c, d) -> (fun session -> compare_duration c (Session.duration session) d)
			| Session_nb_tasks (c, i) -> (fun session -> compare c (List.length (Session.tasks session)) i)
		in
		exists_session fn
	
	let log filter =
		let fn = match filter with
			| Log_date d -> (fun log -> compare_date d (Log.date log))
			| Log_host h -> (fun log -> Log.host log = h)
			| Log_thread_id id -> (fun log -> Log.thread_id log = id)
			| Log_thread_name n -> (fun log -> Util.apply (fun name -> name = n) false (Log.thread_name log))
			| Log_level l -> (fun log -> Log.level log = l)
			| Log_key k -> (fun log -> Log.key log = k)
			| Log_exception -> (fun log -> Log.error log = Log.Got_exception)
			| Log_error -> (fun log -> Log.error log = Log.Got_error)
			| Log_gap_duration (c, d) -> (fun log -> compare_duration c (Log.gap log) d)
			| Log_msg msg -> (fun log -> Util.regexp_match msg (Log.msg log))
		in
		exists_log fn
	
	let item = function
		| Log l -> log l
		| Task t -> task t
		| Session s -> session s
	
	let rec create = function
		| Not t -> (fun x -> not ((create t) x))
		| All -> (fun _ -> true)
		| Item i -> item i
		| Or tl -> (fun x -> List.exists (fun filter -> create filter x) tl)
		| And tl -> (fun x -> List.for_all (fun filter -> create filter x) tl)
end

module rec ITask : Item with type item = Base.task =
struct
	type item = Base.task
	let filter_task filter i = FTask.create filter i
	let to_logs = Task.logs
	let to_tasks t = [t]
	let to_sessions t = match Task.session t with
		| Some s -> [s]
		| None -> []
end
and FTask : Filter with type item = Base.task = Make(ITask)

module ILog =
struct
	type item = Base.log
	let filter_task = FTask.create
	let to_logs l = [l]
	let to_tasks = Log.tasks
	let to_sessions l =
		let aux accu x =
			match Task.session x with
			| Some s -> s :: accu
			| None -> accu in
		List.fold_left aux [] (Log.tasks l)
end
module FLog = Make(ILog)

module ISession =
struct
	type item = Base.session
	let filter_task = FTask.create
	let to_logs s =
		let aux accu task = List.rev_append (Task.logs task) accu in
		List.fold_left aux [] (Session.tasks s)
	let to_tasks s = Session.tasks s
	let to_sessions s = [s]
end
module FSession = Make(ISession)

let task f = FTask.create f
let log f = FLog.create f
let session f = FSession.create f
