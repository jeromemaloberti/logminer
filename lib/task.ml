(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
(* Author: Thomas Gazagnaire <thomas.gazagnaire@citrix.com>     *)

type level = {
	mutable debug: int;
	mutable warn: int;
	mutable info: int;
	mutable error: int }

type stats = {
	mutable nb_children: int;
	mutable gap: Date.Duration.t;
	lines: int;
	errors: int;
	exceptions: int;
	level: level;
	keys: (string * int) list }

type 'a session =
	| Session of 'a Session.t
	| Trackid of string
	| Undef

and 'a parent =
	| No_parent
	| Ref of string
	| Task_to_update of 'a t (* task not globally updated *)
	| Task of 'a t (* task globally updated *)

(* 'a is filter type *)
and 'a t = {
	mutable cache: ('a * bool) list;
	mutable session: 'a t session;
	mutable depth: int;
	host: string;
	thread_id: int;
	task_ref: string;
	task_name: string;
	mutable task_uuid: string option;
	mutable proper_creation: bool;
	mutable proper_destruction: bool;
	mutable creation: Date.t;
	mutable destruction: Date.t;
	is_in_db: bool;
	mutable parent: 'a parent;
	mutable children: 'a t list;
	mutable stats: stats;
	mutable total_stats: stats;
	mutable logs: 'a t Log.t list }

let string_of_task t = Printf.sprintf "(%s, %i, %s)<%i>" t.host t.thread_id t.task_name (List.length t.logs)

let string_of_parent = function
	| No_parent -> "no_parent"
	| Ref s -> Printf.sprintf "ref(%s)" s
	| Task_to_update t -> Printf.sprintf "task_to_update(%s)" (string_of_task t)
	| Task t -> Printf.sprintf "task_to_update(%s)" (string_of_task t)

let empty_stats () = {
	nb_children = 0;
	lines = 0;
	errors = 0;
	exceptions = 0;
	level = {
		debug = 0;
		info = 0;
		error = 0;
		warn = 0 };
	keys = [];
	gap = Date.Duration.zero }

let stats_of_log log =
	let level = match Log.level log with
		| Log.Debug -> { debug = 1; info = 0; warn = 0; error = 0 }
		| Log.Info -> { debug = 0; info = 1; warn = 0; error = 0 }
		| Log.Warn -> { debug = 0; info = 0; warn = 1; error = 0 }
		| Log.Error -> { debug = 0; info = 0; warn = 0; error = 1 }
	in {
		nb_children = 0;
		lines = 1;
		errors = if Log.error log = Log.Got_error then 1 else 0;
		exceptions = if Log.error log = Log.Got_exception then 1 else 0;
		level = level;
		keys = [ (Log.key log, 1) ];
		gap = Date.Duration.zero }

let from_log log =
	match Log.task_ref log, Log.task_name log with
	| None, None -> None
	| Some task_ref, Some task_name ->
		let t = {
			cache = [];
			depth = 0;
			host = Log.host log;
			session = Undef;
			thread_id = Log.thread_id log;
			task_ref = task_ref;
			task_name = task_name;
			task_uuid = None;
			creation = Log.date log;
			destruction = Log.date log;
			proper_creation = false;
			proper_destruction = false;
			is_in_db = task_ref.[0] = 'R';
			parent = No_parent;
			children = [];
			stats = stats_of_log log;
			total_stats = empty_stats ();
			logs = [] } in
		Some t
	| _ -> assert false

let create ~task_ref ~task_name ~task_uuid ~trackid ~log =
	let t = {
		cache = [];
		depth = 0;
		host = Log.host log;
		session = (match trackid with None -> Undef | Some t -> Trackid t);
		thread_id = Log.thread_id log;
		task_ref = task_ref;
		task_name = task_name;
		task_uuid = task_uuid;
		creation = Log.date log;
		destruction = Log.date log;
		proper_creation = false;
		proper_destruction = false;
		is_in_db = task_ref.[0] = 'R';
		parent = No_parent;
		children = [];
		stats = empty_stats ();
		total_stats = empty_stats ();
		logs = [] } in
	t

let exists_among_children f task =
	let rec aux parent = function
		| [] -> false
		| h :: t -> f h || aux h h.children || aux parent t
	in
	f task || aux task task.children

let exists_among_parents f task =
	let rec aux = function
		| Task t -> f t || aux t.parent
		| _ -> false
	in
	aux (Task task)

let depth t = t.depth

let get_exceptions t = t.stats.exceptions
let get_errors t = t.stats.errors
let get_lines t = t.stats.lines
let get_nb_total_children t = t.total_stats.nb_children
let get_level t = function
	| Log.Debug -> t.stats.level.debug
	| Log.Warn -> t.stats.level.warn
	| Log.Info -> t.stats.level.info
	| Log.Error -> t.stats.level.error
let get_key t key =
	if List.mem_assoc key t.stats.keys
	then List.assoc key t.stats.keys
	else 0

let add_key table key v =
	let current =
		if List.mem_assoc key table
		then List.assoc key table
		else 0 in
	(key, current + v) :: List.filter (fun (k, _) -> k <> key) table

let merge_stats s1 s2 = {
	nb_children = s1.nb_children + s2.nb_children;
	lines = s1.lines + s2.lines;
	errors = s1.errors + s2.errors;
	exceptions = s1.exceptions + s2.exceptions;
	level = { debug = s1.level.debug + s2.level.debug;
		info = s1.level.info + s2.level.info;
		warn = s1.level.warn + s2.level.warn;
		error = s1.level.error + s2.level.error };
	gap = Date.Duration.max s1.gap s2.gap;
	keys = List.fold_left (fun accu (k, v) -> add_key accu k v) s2.keys s1.keys }

let parent_of_task_ref = function
	| None -> No_parent
	| Some task_ref -> Ref task_ref

let is_in_db t = t.is_in_db
let host t = t.host
let task_ref t = t.task_ref
let task_uuid t = t.task_uuid
let task_name t = t.task_name
let thread_id t = t.thread_id

let children t = t.children

let has_no_parent t = t.parent = No_parent

let set_no_parent t =
	t.parent <- No_parent

let set_parent_to_ref t r =
	t.parent <- Ref r

let set_parent_to_update t p =
	t.parent <- Task_to_update p

let set_parent t p =
	t.parent <- (Task p);
	p.children <- t :: p.children

let parent t =
	match t.parent with
	| Task t -> Some t
	| _ -> None

let not_completed t = t.proper_creation && not t.proper_destruction

let set_created t = t.proper_creation <- true

let set_destroyed t = t.proper_destruction <- true

let add_log t l =
	t.logs <- l :: t.logs;
	Log.add_task l t;
	let stats = stats_of_log l in
	t.stats <- merge_stats t.stats stats

let logs t = List.rev t.logs

let gap t = t.stats.gap

let set_trackid t trackid =
	match t.session, trackid with
	| Undef, Some trackid -> t.session <- Trackid trackid
	| Undef, None -> ()
	| _ -> failwith "trackid already defined"

let trackid t =
	match t.session with
	| Undef -> None
	| Trackid t -> Some t
	| Session s -> Some (Session.trackid s)

let session t =
	match t.session with
	| Session s -> Some s
	| Undef | Trackid _ -> None

let set_session t s =
	match t.session with
	| Undef | Trackid _ -> t.session <- (Session s)
	| Session _ -> failwith "session already set"

let uname t =
	match t.session with
	| Session s -> Some (Session.uname s)
	| Undef | Trackid _ -> None

let duration t = Date.sub t.destruction t.creation

let compare a b = Date.compare a.creation b.creation

let filter_children f task =
	(* update children *)
	let rec aux accu = function
		| [] -> List.sort compare accu
		| h :: t when f h -> aux (h :: accu) t
		| h :: t -> aux accu (List.rev_append h.children t)
	in
	task.children <- List.sort compare (aux [] task.children);
	List.iter (fun x -> x.parent <- Task task) task.children;
	
	(* update parent *)
	let rec aux = function
		| Task parent when f parent -> task.parent <- Task parent
		| Task parent -> aux parent.parent
		| _ -> task.parent <- No_parent
	in
	aux task.parent

let update_gaps logs =
	let rec aux = function
		| [] | [_] -> ()
		| h1 :: h2 :: t ->
			let gap = Date.sub h2.Log.date h1.Log.date in
			h1.Log.gap <- max h1.Log.gap gap;
			h2.Log.gap <- max h2.Log.gap gap;
			aux (h2 :: t)
	in
	aux logs

let update_stats tasks =
	let roots = List.filter (fun task -> task.parent = No_parent) tasks in
	let rec aux depth gap task =
		let gap = max gap (Log.compute_gap task.logs) in
		List.iter (aux (depth + 1) gap) task.children;
		task.depth <- depth;
		task.logs <- List.sort Log.compare task.logs;
		update_gaps task.logs;
		task.stats.gap <- gap;
		task.children <- List.sort compare task.children;
		task.stats.nb_children <- List.length task.children;
		task.creation <- List.fold_left (fun accu l -> min accu (Log.date l)) task.creation task.logs;
		task.creation <- List.fold_left (fun accu t -> min accu t.creation) task.creation task.children;
		task.destruction <- List.fold_left (fun accu t -> max accu (Log.date t)) task.destruction task.logs;
		task.destruction <- List.fold_left (fun accu t -> max accu t.destruction) task.destruction task.children;
		let total_stats = List.fold_left merge_stats task.stats (List.map (fun c -> c.total_stats) task.children) in
		task.total_stats <- total_stats;
	in
	List.iter (aux 0 Date.Duration.zero) roots

let creation t = t.creation
let destruction t = t.destruction
    
