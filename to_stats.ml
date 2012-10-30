(* Copyright (c) Citrix Systems 2008-2009. All rights reserved. *)
(* Author: Thomas Gazagnaire <thomas.gazagnaire@citrix.com>     *)

open Printf

module Session = struct
	let from_db db = failwith "TODO"
end

module Log = struct
	let max_duration = Date.Duration.of_string "1s"
	let max_density = 100

	type t = {
		starting_date: Date.t;
		mutable ending_date: Date.t;
		mutable density: int;
	}

	let create d = { starting_date = d; ending_date = d; density = 1 }

	let string_of_log (accu, t) l =
		let date = Log.date l in
		if Date.sub date t.starting_date > max_duration || t.density >= max_density then begin
			t.ending_date <- date;
			(t :: accu, create date)
		end else begin
			t.density <- t.density + 1;
			(accu, t)
		end
		
	let output t =
		let duration = Date.Duration.to_float (Date.sub t.ending_date t.starting_date) in
		Printf.printf "%.0f %.0f %f\n" (Date.to_float t.starting_date) (Date.to_float t.ending_date) (float_of_int t.density /. duration)
		
	let from_db db =
		match db.Db.logs with
		| [] -> ()
		| h::t ->
			print_endline "# set xdata time";
			print_endline "# set format x \"%H:%M:%S\"";
			print_endline "# set timefmt \"%s\"";
			print_endline "# plot 'log.data' using 1:3 with boxes";
			let date = Log.date h in
			let data, _ = List.fold_left string_of_log ([], create date) t in
			List.iter output data
end

module Task = struct
	let min_duration = 1.
	let min_lines = 100

	type t = {
		mutable duration: float;
		mutable lines: int;
		mutable occurences: int;
	}
	let zero = { duration = 0.; lines = 0; occurences = 0 }

	let total = { duration = 0.; lines = 0; occurences = 0 }

	let tasks = Hashtbl.create 256

	let string_of_task t =
		let name = Task.task_name t in
		let current = if Hashtbl.mem tasks (Task.task_name t) then Hashtbl.find tasks name else zero in
		let duration = Date.Duration.to_float (Task.duration t) in
		let lines = Task.get_lines t in
		total.duration <- total.duration +. duration;
		total.lines <- total.lines + lines;
		total.occurences <- total.occurences + 1;
		Hashtbl.replace tasks name { duration = current.duration +. duration; lines = current.lines + lines; occurences = current.occurences + 1 }

	let output () =
		let tasks = Hashtbl.fold (fun name t accu -> if t.duration <= min_duration && t.lines < min_lines then accu else (name, t) :: accu) tasks [] in
		let tasks = List.sort (fun (_,x) (_,y) -> compare y.duration x.duration) tasks in
		printf "#%39s %6s %11s %11s %11s %11s\n" "task-name" "#" "duration(s)" "duration(%)" "lines" "lines(%)";
		List.iter (fun (name, t) -> printf "'%38s' %6i %11.1f %11.2f %11i %11.2f\n" name t.occurences t.duration (t.duration *. 100. /. total.duration) t.lines (float_of_int t.lines *. 100. /. float_of_int total.lines)) tasks

	let from_task_list tasks =
		List.iter string_of_task tasks;
		output ()

	let from_db db =
		from_task_list db.Db.tasks
end
