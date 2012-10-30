(* Copyright (c) Citrix Systems 2009. All rights reserved.  *)
(* Author: Thomas Gazagnaire <thomas.gazagnaire@citrix.com> *)

(* This module is used for printing audit files. *)

let session f t =
	match Task.session t with
	| Some s -> f s
	| None -> "?"

let uname_of_task t = session Session.uname t
let auth_of_task t = session Session.auth_user_sid t
let is_superuser_of_task t = session (fun s -> string_of_bool (Session.is_local_superuser s)) t

(* TODO: give the parent-child relationship by adding two fields: the taskID and its parent task-ID *)
module Task =
struct
	let protect s =
		for i = 0 to String.length s - 1 do
			if s.[i] = '"' then s.[i] <- '\''
		done;
		s
	
	let message_of_task t =
		let messages = Task.logs t in
		let log =
			if List.length messages < 3 then
				List.nth messages ((List.length messages) - 1)
			else if Util.startswith "Async" (Task.task_name t) then
				List.nth messages 3
			else
				List.nth messages 2 in
		protect (Log.msg log)
	
	let string_of_task t =
		Printf.sprintf "%s,%s,%s,%s,%s,\"%s\""
			(Date.String_of.t (Task.creation t))
			(uname_of_task t)
			(is_superuser_of_task t) t.Task.host (Task.task_name t)
			(message_of_task t)
	
	let from_task_list tasks =
		Printf.printf "date,user name,is local superuser,host name,task name,message\n";
		List.iter (fun task -> print_endline (string_of_task task)) tasks
	
	let from_db db =
		from_task_list db.Db.tasks
end

module Log =
struct
	let string_of_task name =
		To_text.string_of_option name
	
	let aux f l =
		let res = List.map f (Log.tasks l) in
		let res = List.filter ((<>) "?") res in
		if res <> [] then
			Printf.sprintf "{ %s }" (String.concat "," res)
		else
			"?"
	
	let uname_of_log l = aux uname_of_task l
	let is_superuser_of_log l = aux is_superuser_of_task l
	
	let string_of_log l =
		Printf.sprintf "%s,%s,%s,%s,%s,%s"
			(Date.String_of.t (Log.date l)) (uname_of_log l)
			(is_superuser_of_log l)
			l.Log.host (string_of_task (Log.task_name l)) (Log.msg l)
	
	let from_log_list logs =
		Printf.printf "date,user name,is local superuser,host name,task name, message\n";
		List.iter (fun log -> print_endline (string_of_log log)) logs
	
	let from_db db =
		from_log_list db.Db.logs
end
