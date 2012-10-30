(* Copyright (c) Citrix Systems 2008-2009. All rights reserved. *)
(* Author: Thomas Gazagnaire <thomas.gazagnaire@citrix.com>     *)

open Printf

module Session = struct
	let string_of_session s =
		printf "<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>\n"
			(Session.trackid s)
			(To_text.get_uname (Session.uname s))
			(To_text.Session.string_of_superuser s)
			(Date.String_of.t ~verbose: (To_text.is_verb ()) (Session.creation s))
			(Date.Duration.to_string ~verbose: (To_text.is_verb ()) (Session.duration s))
			(To_text.Session.string_of_stats s)

	let from_db db =
		printf "<table>\n";
		List.iter string_of_session db.Db.sessions;
		printf "</table>\n"
end

module Log = struct
	let string_of_thread id name =
		let s = To_text.string_of_option name in
		sprintf "%i</td><td>%s" id s
	
	let string_of_task name r =
		sprintf "%s</td><td>%s" (To_text.string_of_option name) (To_text.string_of_option r)

	let color_of_gap l =
		if l.Log.tasks = [] then "FF"
		else begin
			let d = Log.gap l in
			let max = List.fold_left (fun accu t -> max accu (Date.Duration.to_float (Task.gap t))) 0. l.Log.tasks in
			let i = int_of_float ( (Date.Duration.to_float d) /. max *. 255.) in
			let i1 = 15 - (i mod 16) in
			let i2 = 15 - (i/16) in
			let conv x = if x < 10 then (sprintf "%i" x).[0] else char_of_int (Char.code 'A' + x - 10) in
			sprintf "%c%c" (conv i2) (conv i1)
		end

	let string_of_log l =
		let color = color_of_gap l in
		printf "<tr bgcolor=\"#FF%s%s\"><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>\n"
			color color
			(Date.String_of.t ~verbose:true ~sep:"</td><td>" (Log.date l)) (To_text.string_of_level l.Log.level) l.Log.host (string_of_thread (Log.thread_id l) (Log.thread_name l))
			(string_of_task (Log.task_name l) (Log.task_ref l)) (Log.key l) (Log.msg l)
	
	let from_log_list logs =
		printf "<table>\n";
		List.iter string_of_log logs;
		printf "</table>\n"
	
	let rec from_db db =
		from_log_list db.Db.logs
end

module Task = struct
	let string_of_task t =
		sprintf "%s %s" (Task.task_name t) (Task.task_ref t)

	let from_task_list tasks =
		let children_of t = Task.children t in
		let roots = List.filter (fun t -> Task.parent t = None) tasks in
		Util.Tree.dump_to_html ~children_of ~name_of:string_of_task ~roots

	let from_db db =
		from_task_list db.Db.tasks
end

		
