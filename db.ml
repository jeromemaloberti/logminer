(* Copyright (c) Citrix Systems 2008-2009. All rights reserved. *)
(* Author: Thomas Gazagnaire <thomas.gazagnaire@citrix.com>     *)

type db = {
	logs: Filter.Base.log list;
	tasks: Filter.Base.task list;
	sessions: Filter.Base.session list }

let make ~logs ~tasks ~sessions = {
	logs = logs;
	tasks = tasks;
	sessions = sessions }

let filter f db =
	let new_db = {
		logs = List.filter (Filter.log f) db.logs;
		tasks = List.filter (Filter.task f) db.tasks;
		sessions = List.filter (Filter.session f) db.sessions } in
	List.iter (Task.filter_children (Filter.task f)) new_db.tasks;
	Task.update_stats new_db.tasks;
	{ new_db with tasks = List.sort Task.compare new_db.tasks }

let to_backup_file (db: db) filename =
	let save = open_out filename in
	Printf.printf "%s%!" (Util.left Util.size_of_terminal
				(Printf.sprintf "Saving %i log lines, %i tasks and %i sessions into %s"
						(List.length db.logs)
						(List.length db.tasks)
						(List.length db.sessions)
						filename));
	Marshal.to_channel save db [];
	close_out save;
	Printf.printf " [OK]\n%!"

let from_backup_file (f: Filter.t) filename =
	try
		let save = open_in filename in
		filter f (Marshal.from_channel save: db)
	with e ->
		Printf.eprintf "Error: %s\n" (Printexc.to_string e);
		exit 2
