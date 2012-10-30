(* Copyright (c) Citrix Systems 2008-2009. All rights reserved. *)
(* Author: Thomas Gazagnaire <thomas.gazagnaire@citrix.com>     *)

open Printf

let auto_resize = ref true
let max_task_name_size = ref 50
let max_host_name_size = ref 15

let verbose_level = ref 0

let verbose i () =
	verbose_level := i

(* level 1*)
let if_verb s t =
	if !verbose_level >= 1
	then s
	else t

(* level 2*)
let if_verbose s t =
	if !verbose_level >= 2
	then s
	else t

let is_verb () = !verbose_level >= 1
let is_verbose () = !verbose_level >= 2

let get_uname s = if s = "" then "?" else s
let string_of_option ?(default="") o = match o with
	| None -> default
	| Some s -> get_uname  s

let string_of_level = function
	| Log.Debug -> "debug"
	| Log.Warn -> " warn"
	| Log.Info -> " info"
	| Log.Error -> "error"

type col_size = { current : int; max : int }
let make_col x y = { current = x; max = y }
let col_size c = min c.current c.max

module Session =
struct
	type env = {
		trackid : col_size;
		uname : col_size;
		root : col_size;
		creation : col_size;
		duration : col_size;
		stats : col_size }
	
	let create_env () = {
		trackid = make_col 32 32;
		uname = make_col 0 20;
		root = make_col 0 10;
		creation = make_col 17 17;
		duration = make_col 0 17;
		stats = make_col 0 10 }
	
	let string_of_superuser s =
		if Session.is_local_superuser s then "(root)" else "(user)"
	
	let string_of_stats s =
		sprintf "%i" (List.length (Session.tasks s))
	
	let string_of_session env s =
		sprintf "%s %s %s %s %s %s"
			(Session.trackid s)
			(Util.left (col_size env.uname) (get_uname (Session.uname s)))
			(Util.right (col_size env.root) (string_of_superuser s))
			(Util.right (col_size env.creation) (Date.String_of.t ~verbose: (is_verb ()) (Session.creation s)))
			(Util.left (col_size env.duration) (Date.Duration.to_string ~verbose: (is_verb ()) (Session.duration s)))
			(Util.left (col_size env.stats) (string_of_stats s))
	
	let update_env env s =
		let aux r n = { r with current = max r.current n } in
		let uname = get_uname (Session.uname s) in
		let duration = Date.Duration.to_string ~verbose: (is_verb ()) (Session.duration s) in
		let root = string_of_superuser s in
		let stats = string_of_stats s in
		{ env with
			uname = aux env.uname (String.length uname);
			root = aux env.root (String.length root);
			duration = aux env.duration (String.length duration);
			stats = aux env.stats (String.length stats) }
	
	let from_db db =
		let env =
			if !auto_resize
			then List.fold_left update_env (create_env ()) db.Db.sessions
			else create_env () in
		List.iter (fun s -> printf "%s\n" (string_of_session env s)) db.Db.sessions
end

module Task =
struct
	type env = {
		task_name : col_size;
		user_name : col_size;
		thread_id : col_size;
		host_name : col_size;
		date : col_size;
		duration : col_size;
		lines : col_size;
		lines_total : col_size;
		level : col_size;
		level_total : col_size;
		errors : col_size;
		errors_total : col_size;
		children : col_size }
	
	let min_task_name = 7
	
	let create_env () = {
		task_name = make_col min_task_name !max_task_name_size;
		user_name = make_col 0 30;
		thread_id = make_col 0 8;
		host_name = make_col 0 !max_host_name_size;
		date = make_col 17 17;
		duration = make_col 0 15;
		lines = make_col 0 6;
		lines_total = make_col 0 10;
		level = make_col 0 6;
		level_total = make_col 0 12;
		errors = make_col 0 6;
		errors_total = make_col 0 12;
		children = make_col 0 6 }
	
	let string_of_date env starting ending =
		sprintf "%s%s %s"
			(Date.String_of.t ~verbose: (is_verb ()) starting)
			(if_verbose (sprintf " %s" (Date.String_of.t ~verbose: true ending)) "")
			(Util.right (col_size env.duration) (Date.Duration.to_string ~verbose: (is_verb ()) (Date.sub ending starting)))
	
	let string_of_level l =
		let aux num unit =
			if num <> 0
			then sprintf "%i%s" num unit
			else ""
		in
		sprintf "%s%s%s%s"
			(if_verbose (aux l.Task.debug "d") "")
			(if_verbose (aux l.Task.info "i") "")
			(aux l.Task.warn "w")
			(aux l.Task.error "e")
	
	let string_of_errors s =
		if_verb
			(sprintf "%i/%i" s.Task.exceptions s.Task.errors)
			(if s.Task.errors > 0 then "x" else "-")
	
	let string_of_thread_id env id =
		Util.right (col_size env.thread_id) (string_of_int id)
	
	let tab_of_depth t =
		let d = Task.depth t in
		Util.Tree.tab_of_depth d
	
	let pretty_task_name t =
		let not_completed = Task.not_completed t in
		if not_completed
		then sprintf "*'%s'" (Task.task_name t)
		else sprintf "'%s'" (Task.task_name t)
	
	(* the tree display will add the tab automatically, so need to remove them when computing the size of the column *)
	let string_of_task_name env t =
		let tab = tab_of_depth t in
		let col = max (col_size env.task_name - tab) min_task_name in
		Util.left col (pretty_task_name t)
	
	let string_of_short_stats env ?(total = false) s =
		let col_lines = if total then col_size env.lines_total else col_size env.lines in
		let col_errors = if total then col_size env.errors_total else col_size env.errors in
		let col_level = if total then col_size env.level_total else col_size env.level in
		sprintf "%s%s%s"
			(if_verb (Printf.sprintf "%sl " (Util.right col_lines (string_of_int s.Task.lines))) "")
			(Util.right col_errors (string_of_errors s))
			(if_verb (Printf.sprintf " %s" (Util.right col_level (string_of_level s.Task.level))) "")
	
	let string_of_extra_stats env t =
		let nb_children = Task.get_nb_total_children t in
		if nb_children > 0
		then begin
			if_verbose
				(sprintf "total: %s" (string_of_short_stats env ~total: true t.Task.total_stats))
				(if_verb (Util.right (col_size env.children) (string_of_int nb_children)) "")
		end else ""
	
	let string_of_stats env t =
		sprintf "%s %s"
			(string_of_short_stats env t.Task.stats)
			(string_of_extra_stats env t)
	
	let string_of_name env t =
		sprintf "%s %s %s %s %s"
			(string_of_task_name env t)
			(Util.left (col_size env.user_name) (string_of_option ~default:"?" (Task.uname t)))
			(Util.left (col_size env.host_name) t.Task.host)
			(string_of_thread_id env t.Task.thread_id)
			t.Task.task_ref
	
	let string_of_task env t =
		sprintf "%s %s %s"
			(string_of_name env t)
			(string_of_date env (Task.creation t) (Task.destruction t))
			(string_of_stats env t)
	
	let count = ref 0
	let update_env env t =
		let task_name_size = tab_of_depth t + String.length (pretty_task_name t) in
		let update r s = { r with current = max r.current (String.length s) } in
		{ env with
			task_name = { env.task_name with current = max env.task_name.current task_name_size };
			user_name = update env.user_name (string_of_option ~default:"?" (Task.uname t));
			thread_id = update env.thread_id (string_of_int t.Task.thread_id);
			host_name = update env.host_name t.Task.host;
			duration = update env.duration (Date.Duration.to_string ~verbose: (is_verb()) (Task.duration t));
			children = if_verb (update env.children (string_of_int (Task.get_nb_total_children t))) env.children;
			lines = if_verb (update env.lines (string_of_int t.Task.stats.Task.lines)) env.lines;
			lines_total = if_verbose (update env.lines_total (string_of_int t.Task.total_stats.Task.lines)) env.lines_total;
			level = if_verb (update env.level (string_of_level t.Task.stats.Task.level)) env.level;
			level_total = if_verbose (update env.level_total (string_of_level t.Task.total_stats.Task.level)) env.level_total;
			errors = update env.errors (string_of_errors t.Task.stats);
			errors_total = if_verbose (update env.errors_total (string_of_errors t.Task.total_stats)) env.errors }
	
	let from_task_list tasks : unit =
		let env = if !auto_resize then begin
				printf "resizing %i tasks\n%!" (List.length tasks);
				List.fold_left update_env (create_env ()) tasks end else create_env ()in
		let children_of t = Task.children t in
		let roots = List.filter (fun t -> Task.parent t = None) tasks in
		Util.Tree.dump ~children_of ~name_of: (string_of_task env) ~roots
	
	let rec from_db db =
		from_task_list db.Db.tasks
end

module Log =
struct
	let string_of_thread id name =
		let s = string_of_option name in
		if s = "" then sprintf "%i" id else sprintf "%i %s" id s
	
	let string_of_task name r =
		let s = Printf.sprintf "%s %s" (string_of_option name) (string_of_option r) in
		if s = " " then "" else s
	
	let string_of_log l =
		sprintf "[%s|%s|%s|%s|%s|%s] %s"
			(Date.String_of.t ~verbose:true (Log.date l)) (string_of_level l.Log.level) l.Log.host (string_of_thread (Log.thread_id l) (Log.thread_name l))
			(string_of_task (Log.task_name l) (Log.task_ref l)) (Log.key l) (Log.msg l)
	
	let from_log_list logs =
		List.iter (fun log -> print_endline (string_of_log log)) logs
	
	let rec from_db db =
		from_log_list db.Db.logs
end

