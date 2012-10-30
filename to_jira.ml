(* Copyright (c) Citrix Systems 2008-2009. All rights reserved. *)
(* Author: Thomas Gazagnaire <thomas.gazagnaire@citrix.com>     *)

open Printf

module Task =
struct
	let tree = "*"
	let shift s = "*" ^ s
	
	let string_of_stats s =
		let aux i = if i = 0 then "--" else sprintf "{color:red}%i{color}" i in
		sprintf "%i|%s|%s" s.Task.lines (aux s.Task.level.Task.warn) (aux s.Task.level.Task.error)
	
	let string_of_date creation destruction =
		sprintf "%s|%s"
			(Date.String_of.t creation)
			(Date.Duration.to_string (Date.sub destruction creation))
	
	let string_of_task tab t =
		let name =
			sprintf "%s *%s*|%s|%s|%i"
				tab t.Task.task_name t.Task.task_ref t.Task.host t.Task.thread_id
		in
		let date = string_of_date (Task.creation t) (Task.destruction t) in
		let stats =
			sprintf "%s|%s"
				(string_of_stats t.Task.stats)
				(if t.Task.stats <> t.Task.total_stats
					then sprintf "%i" (Task.get_nb_total_children t)
					else "--")
		in
		sprintf "|%s|%s|%s|" name date stats
	
	let from_task_list tasks : unit =
		let rec aux tab t =
			if List.mem t tasks && t.Task.task_ref <> "" then
				printf "%s\n" (string_of_task tab t);
			List.iter (aux (if tab ="" then tree else shift tab)) t.Task.children
		in
		printf "||task name||task ref||host||thread id||date||duration||lines||warnings||errors||children||\n";
		List.iter (aux "") tasks
	
	let from_db db =
		from_task_list db.Db.tasks
end

module Log =
struct
	let string_of_thread id name =
		sprintf "%i|%s" id (To_text.string_of_option name)
	
	let string_of_task name r =
		sprintf "%s|%s" (To_text.string_of_option name) (To_text.string_of_option r)
	
	let string_of_log l =
		sprintf "|%s|%s|%s|%s|%s|%s|%s|"
			(Date.String_of.t (Log.date l)) (To_text.string_of_level l.Log.level) l.Log.host (string_of_thread (Log.thread_id l) (Log.thread_name l))
			(string_of_task (Log.task_name l) (Log.task_ref l)) (Log.key l) (Log.msg l)
	
	let from_log_list logs =
		printf "||date||level||host||thread-id||thread-name||task-name||task-ref||key||message||\n";
		List.iter (fun log -> print_endline (string_of_log log)) logs
	
	let from_db db =
		from_log_list db.Db.logs
end
