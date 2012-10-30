(* Copyright (c) Citrix Systems 2008-2009. All rights reserved. *)
(* Author: Thomas Gazagnaire <thomas.gazagnaire@citrix.com>     *)

open Printf

let quiet = ref false

let info (fmt: ('a, unit, string, unit) format4) : 'a =
	kprintf (fun s -> if not !quiet then print_string s; flush stdout) fmt

let debug (fmt: ('a, unit, string, unit) format4) : 'a =
	kprintf (fun s -> print_endline s; flush stdout) fmt

(*********************************************************************************)
(* Change the following functions if the format of xapi logging function changes *)
(*********************************************************************************)
let short_reference_size = 14

let task_begins_creation = Str.regexp "task \\(.*\\) created\\( (trackid=\\(.*\\))\\)?\\( by .*\\)?"
let task_begins_forward = Str.regexp "task \\(.*\\) forwarded\\( (trackid=\\(.*\\))\\)?"
let task_begins_async = Str.regexp "spawning a new thread to handle the current task\\( (trackid=\\(.*\\))\\)?"
let task_ends = Str.regexp "task destroyed\\|forwarded task destroyed\\|nothing more to process for this thread"
let is_async_task = Util.startswith "Async"

let uuid_size = 36
let remove_uuid s =
	try let rindex = String.rindex s '(' in
		if s.[rindex + 1] = 'u' && s.[rindex + 2] = 'u' && s.[rindex + 3] = 'i' && s.[rindex + 4] = 'd' && s.[rindex + 5] = ':'
		then String.sub s 0 (rindex - 1), Some (String.sub s (rindex + 6) uuid_size)
		else s, None
	with _ -> s, None

let get_reference s =
	let s, _ = remove_uuid s in
	let n = String.length s in
	String.sub s (n - short_reference_size) short_reference_size

let get_reference_name_and_uuid s =
	let s, uuid = remove_uuid s in
	let n = String.length s in
	String.sub s (n - short_reference_size) short_reference_size,
	String.sub s 0 (n - short_reference_size - 1),
	uuid

let is_a_dispatch_task = function
	| None -> false
	| Some t -> Util.startswith "dispatch:" (Task.task_name t)

let dispatch_task s =
	let n = String.length s in
	let k = String.length "dispatch:" in
	String.sub s k (n-k)

(*********************************************************************************)

type session = Session_parser.session

type env = {
	log_queue : Filter.Base.log Queue.t;
	mutable log_counter : int;
	mutable last_log_line : Filter.Base.log option;
	mutable aggreg_non_matched_lines : bool;
	
	task_tbl : (string * int * string, Filter.Base.task) Hashtbl.t;
	mutable task_counter : int;
	
	session_tbl : (string, session) Hashtbl.t;
	mutable session_counter : int;
	
	filter_online : bool;
	log_filter : Filter.Base.log -> bool;
	task_filter : Filter.Base.task -> bool;
	session_filter : Filter.Base.session -> bool;
	is_monotonous : bool }

let force_monotonous = ref false

let create_env filter = {
	log_queue = Queue.create ();
	log_counter = 0;
	last_log_line = None;
	aggreg_non_matched_lines = true;
	
	task_tbl = Hashtbl.create 4096;
	task_counter = 0;
	
	session_tbl = Hashtbl.create 1024;
	session_counter = 0;
	
	filter_online = Check.can_filter_online filter;
	log_filter = Filter.log filter;
	task_filter = Filter.task filter;
	session_filter = Filter.session filter;
	is_monotonous = Check.is_monotonous filter || !force_monotonous }

let env_to_db env =
	let logs = List.rev (Queue.fold (fun acc l -> l :: acc) [] env.log_queue) in
	let logs = List.sort Log.compare logs in
	
	let tasks = Hashtbl.fold (fun _ task accu -> task:: accu) env.task_tbl [] in
	let tasks = List.sort Task.compare tasks in
	
	let sessions = Hashtbl.fold (fun _ session accu -> match session with Session_parser.Session s -> s :: accu | Session_parser.Destruction _ -> accu) env.session_tbl [] in
	let sessions = List.sort Session.compare sessions in
	
	Db.make ~logs ~tasks ~sessions

module Update =
struct
	
	(* Update the log queue *)
	let queue_add env log =
		if not env.filter_online then begin
			env.last_log_line <- Some log;
			Queue.add log env.log_queue
		end else if env.log_filter log then begin
			Queue.add log env.log_queue;
			env.aggreg_non_matched_lines <- true;
			env.last_log_line <- Some log
		end else
			env.aggreg_non_matched_lines <- false
	
	let log env l =
		env.log_counter <- env.log_counter + 1;
		queue_add env l
	
	(* Update the session table *)
	let session_creation env s =
		env.session_counter <- env.session_counter + 1;
		let trackid = Session.trackid s in
		if Hashtbl.mem env.session_tbl trackid then begin
			match Hashtbl.find env.session_tbl trackid with
			| Session_parser.Session _ -> debug "Warning: duplicated session creation for trackid: %s" trackid; ()
			| Session_parser.Destruction (_, d) ->
				Session.set_destruction s d;
				Hashtbl.replace env.session_tbl trackid (Session_parser.Session s)
		end else
			Hashtbl.add env.session_tbl trackid (Session_parser.Session s)
	
	let session_destruction env trackid date =
		if Hashtbl.mem env.session_tbl trackid then begin
			match Hashtbl.find env.session_tbl trackid with
			| Session_parser.Session s -> Session.set_destruction s date
			| Session_parser.Destruction _ -> debug "Warning: duplicated session destruction for trackid %s" trackid; ()
		end else
			Hashtbl.add env.session_tbl trackid (Session_parser.Destruction (trackid, date))
	
	let session env = function
		| Some (Session_parser.Session s) -> session_creation env s
		| Some (Session_parser.Destruction (trackid, date)) -> session_destruction env trackid date
		| None -> ()
	
	let update_session_of_task env task =
		match Task.trackid task with
		| None -> ()
		| Some trackid ->
			if Hashtbl.mem env.session_tbl trackid then
				match Hashtbl.find env.session_tbl trackid with
				| Session_parser.Session session ->
					Task.set_session task session;
					Session.add_task session task
				| Session_parser.Destruction _ -> ()
	
	(*---------------------- First update stage of task informations ---------------------------------*)
	(* At this point, the task relationships are not relevant as log lines are not ordered when files *)
	(* are concatenated. Thus, the children, root and total_lines fields are ignored and will be      *)
	(* updated in the second  update stage which takes place later. The parent field contains partial *)
	(* information which will also be used in the second update stage.                                *)
	(*------------------------------------------------------------------------------------------------*)
	let task env = function
		| Some task ->
			let task_ref = Task.task_ref task in
			let host = Task.host task in
			let thread_id = Task.thread_id task in
			if not (Hashtbl.mem env.task_tbl (host, thread_id , task_ref))
			then begin
				env.task_counter <- env.task_counter + 1;
				if not env.filter_online || env.task_filter task then
					Hashtbl.add env.task_tbl (host, thread_id, task_ref) task;
			end
		| None -> ()
	
	(* ----------------------Second update stage ----------------------------------------------------*)
	(* We make the assumption that the last task_ref appearing in the log on a different host/thread *)
	(* is ALWAYS the father of the current task_ref. Even if the task is already finished on this    *)
	(* host/thread (because it happens in the asynchronous case), as we assume that a task will log  *)
	(* something before being  forwarded again...                                                    *)
	(* --------------------------------------------------------------------------------------------- *)
	let update_task_parents tasks =
		let memory = Hashtbl.create 1024 in
		let update task =
			begin match task.Task.parent with
				| Task.Ref parent_ref ->
					if Hashtbl.mem memory parent_ref
					then begin
						let last = Hashtbl.find memory parent_ref in
						Task.set_parent task last;
					end else
						task.Task.parent <- Task.No_parent
				| Task.Task_to_update last ->
					Task.set_parent task last
				| Task.Task _ | Task.No_parent -> ()
			end;
			Hashtbl.replace memory (Task.task_ref task) task
		in
		List.iter update tasks
	
	let global_tasks env tasks =
		update_task_parents tasks;
		Task.update_stats tasks;
		List.iter (update_session_of_task env) tasks
end

let msg_add env line =
	match env.last_log_line with
	| Some log when env.aggreg_non_matched_lines -> Queue.add line log.Log.msg
	| _ -> ()

let get n expr =
	try Some (Str.matched_group n expr)
	with Not_found -> None

let log_of_line ?log_counter line =
  let log = Xensource.parse_xensource line in
  match log with None -> None
    | Some (d,h,l,tn,ti,tr,ta,k,e,m) ->
        Some (Log.make d h l tn ti tr ta k e m)

let tasks_of_log cache log =
	let msg = Log.msg log in
	let host = Log.host log in
	let thread_id = Log.thread_id log in
	(* The current task *)
	let task =
		let destruction = Str.string_match task_ends msg 0 in
		let t =
			match Log.task_ref log with
			| None -> None
			| Some task_ref ->
				if Hashtbl.mem cache (Log.host log, Log.thread_id log, task_ref)
				then Some (Hashtbl.find cache (host, thread_id, task_ref))
				else Task.from_log log in
		Util.may
			(fun x ->
					if destruction then Task.set_destroyed x;
					if Task.has_no_parent x then Task.set_parent_to_ref x (Task.task_ref x))
			t;
		t
	in
	(* Created task *)
	let created_task =
		if Str.string_match task_begins_creation msg 0 then begin
			(* If the task is created *)
			let current_task = Str.matched_group 1 msg in
			let task_ref, task_name, task_uuid = get_reference_name_and_uuid current_task in
			let trackid = get 3 msg in
			let t =
				if Hashtbl.mem cache (host, thread_id, task_ref)
				then Hashtbl.find cache (host, thread_id, task_ref)
				else Task.create ~task_ref ~task_name ~task_uuid ~trackid ~log in
			begin try
				let parent_ref = get_reference (Str.matched_group 4 msg) in
				Task.set_parent_to_ref t parent_ref;
			with Not_found ->
				Util.may (fun task -> Task.set_parent_to_update t task) task
			end;
			if not (is_async_task task_name) then Task.set_created t;
			Some t
			
		end else if Str.string_match task_begins_forward msg 0 then begin
			(* If the task is forwarded *)
			let current_task = Str.matched_group 1 msg in
			let task_ref, task_name, task_uuid = get_reference_name_and_uuid current_task in
			let trackid = get 3 msg in
			let t =
				if Hashtbl.mem cache (host, thread_id, task_ref)
				then Hashtbl.find cache (host, thread_id, task_ref)
				else Task.create ~task_ref ~task_name ~task_uuid ~trackid ~log in
			Task.set_parent_to_ref t task_ref;
			Task.set_created t;
			Some t
			
		end else if Str.string_match task_begins_async msg 0 then begin
			(* If the task is asynchronous *)
			let trackid = get 2 msg in
			let t =
				match Log.task_ref log with
				| None -> None
				| Some task_ref ->
					if Hashtbl.mem cache (host, thread_id, task_ref)
					then Some (Hashtbl.find cache (host, thread_id, task_ref))
					else Task.from_log log in
			Util.may
				(fun t ->
						Task.set_trackid t trackid;
						Task.set_created t;
						Util.may (fun task -> Task.set_parent_to_update t task) task)
				t;
			t
			
		end else
			None
	in
	
	(* If the created task is a dispatch one, insert it betweem it its child and and the child's parent *)
	if is_a_dispatch_task task then begin
		match task, created_task with
		| None, _ | _, None -> ()
		| Some task, Some created_task ->
			match created_task.Task.parent with
			| Task.Task_to_update p when p == task && Task.is_in_db created_task ->
				Task.set_parent_to_ref task (Task.task_ref created_task)
			| Task.Task_to_update _ -> ()
			| parent ->
				Task.set_parent_to_update created_task task;
				task.Task.parent <- parent
	end;
	
	Util.may (fun t -> Task.add_log t log) task;
	Util.may (fun t -> Task.add_log t log) created_task;
	task, created_task

let session_created = Str.regexp "Session.create trackid=\\(.*\\) pool=\\(true\\|false\\) uname=\\(.*\\) is_local_superuser=\\(true\\|false\\) auth_user_sid=\\(.*\\)"
let session_destroyed = Str.regexp "Session.destroy trackid=\\(.*\\)"
let session_gc = Str.regexp "Session.destroy _ref=\\(.*\\) (last active .*"

let session_of_log_old log =
	let msg = Log.msg log in
	
	if Str.string_match session_created msg 0 then begin
		(* If the session is created *)
		let trackid = Str.matched_group 1 msg in
		let pool = bool_of_string (Str.matched_group 2 msg) in
		let uname = Str.matched_group 3 msg in
		let is_local_superuser = bool_of_string (Str.matched_group 4 msg) in
		let auth_user_sid = Str.matched_group 5 msg in
		Some (Session_parser.Session (Session.create ~trackid ~pool ~uname ~is_local_superuser ~auth_user_sid ~creation: (Log.date log)))
		
	end else if Str.string_match session_destroyed msg 0 || Str.string_match session_gc msg 0 then begin
		(* if the session is destroyed *)
		let trackid = Str.matched_group 1 msg in
		Some (Session_parser.Destruction (trackid, Log.date log))
		
	end else
		None

let session_of_log log =
  Session_parser.parse_xensource_session (Log.date log) (Log.msg log)

let parse_line cache line =
	match log_of_line line with
	| Some log ->
		let task, created_task = tasks_of_log cache log in
		let session = session_of_log log in
		Some (log, task, created_task, session)
	| None -> None

let parse_and_update_line env line =
	match parse_line env.task_tbl line with
	| Some (log, task, created_task, session) ->
		Update.log env log;
		Update.task env task;
		Update.task env created_task;
		Update.session env session
	| None -> msg_add env line

let info_of_line line =
	match log_of_line line with
	| Some log -> Some (Log.host log, Log.date log)
	| None -> None

let need_checking_file env file =
	let check_line l = match log_of_line l with
		| None -> false
		| Some _ -> true in
	let log_filter l = match parse_line env.task_tbl l with
		| None -> false
		| Some (log, _, _, _) -> env.log_filter log in
	if env.is_monotonous && env.filter_online
	then begin
		Util.with_file file [Unix.O_RDONLY] 0o755
			(fun fd ->
					let start = log_filter (Util.find_first_useful_line check_line fd) in
					let last = log_filter (Util.find_last_useful_line check_line fd) in
					start || last)
	end else
		true

let not_too_big file =
	try ignore (Unix.stat file); true
	with Unix.Unix_error (Unix.EOVERFLOW, _, _) -> false

let get_all_files file =
	let logs_of_gz archive =
		let dir = Util.untar_bugreport archive in
		Util.find_filename_starting_by [ "xensource.log" ] dir
	in
	let logs_of_zips archive =
		let dir = Util.unzip_bugreport archive in
		Util.find_filename_starting_by [ "xensource.log" ] dir
	in try
	
	(* if it is a directory, it should be a bug-CA-whatever created by bugget *)
		if not_too_big file && (Unix.stat file).Unix.st_kind = Unix.S_DIR
		then begin
			let logs = Util.find_filename_starting_by [ "xensource.log" ] file in
			if List.length logs <> 0
			then logs
			else begin
				let gzs = Util.find_filename_having_substr [ ".tar.gz"; ".tgz"; ".tar.bz2" ] file in
				let zips = Util.find_filename_ending_by [ ".zip" ] file in
				info "%s%!"(Util.left Util.size_of_terminal (sprintf "decompressing %i file(s)" (List.length gzs + List.length zips)));
				let f = List.flatten ((List.map logs_of_gz gzs) @ (List.map logs_of_zips zips)) in
				info " [OK]\n%!";
				f
			end
		end
		
		(* if it is a bug report archive *)
		else if Util.endswith ".gz" file
		then logs_of_gz file
		
		(* if it is a singleton log file *)
		else [file]
	
	with e ->
		eprintf "Error: %s is not a valid file\n" file;
		exit 3

let parse_files env files =
	let files = List.concat (List.map get_all_files files) in
	let files = List.sort (Info.compare_file info_of_line) files in
	let n = List.length files in
	let size_n = String.length (string_of_int n) in
	let c = ref 0 in
	List.iter
		(fun file ->
				incr c;
				info "%s%!" (Util.left_with_tail Util.size_of_terminal ~tail: Info.tail_col_file (sprintf "Analyzing log file %s/%i: %s" (Util.right size_n (string_of_int !c)) n file));
				if need_checking_file env file
				then begin
					Util.readfile_line (parse_and_update_line env) file;
					info " [OK]\n%!"
				end else
					info " [SKIP]\n%!")
		files

let to_db filter files =
	let env = create_env filter in
	
	if env.filter_online
	then info "Warning: online filtering is active, task-related data might be inaccurate.\n%!";
	
	if env.is_monotonous
	then begin
		info "Warning: this is monotonous, thus only the first and last lines of a file will be analysed in order to know if it needs to be analyzed.\n";
	end;
	
	parse_files env files;
	
	info "%s%!" (Util.left Util.size_of_terminal
				(Printf.sprintf "Sorting %i log lines, %i tasks and %i sessions"
						(Queue.length env.log_queue)
						(Hashtbl.length env.task_tbl)
						(Hashtbl.length env.session_tbl)));
	let db = env_to_db env in
	
	info " [OK]\n%!";
	info "%s%!" (Util.left Util.size_of_terminal "Updating the task relations");
	
	Update.global_tasks env db.Db.tasks;
	
	let db = Db.filter filter db in
	
	info " [OK]\nInfo: %i%s log lines, %i%s tasks and %i%s sessions matched.\n%!"
		(List.length db.Db.logs)
		(if env.log_counter = List.length db.Db.logs then "" else sprintf "/%i" env.log_counter)
		(List.length db.Db.tasks)
		(if env.task_counter = List.length db.Db.tasks then "" else sprintf "/%i" env.task_counter)
		(List.length db.Db.sessions)
		(if env.session_counter = List.length db.Db.sessions then "" else sprintf "/%i" env.session_counter);
	db
