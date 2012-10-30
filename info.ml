(* Copyright (c) Citrix Systems 2009. All rights reserved.  *)
(* Author: Thomas Gazagnaire <thomas.gazagnaire@citrix.com> *)

type t = {
	host : string; (* the host name at the beginning of the file *)
	first : Date.t;
	last : Date.t;
	duration : Date.Duration.t;
	file : string }

let make ~host ~first ~last ~file = {
	host = host;
	first = first;
	last = last;
	duration = Date.sub last first;
	file = file }

let info_of_file parse_line file =
	let check_line l = match parse_line l with
		| Some _ -> true
		| None -> false in
	Util.with_file file [Unix.O_RDONLY] 0o755
		(fun fd ->
				let first = Util.find_first_useful_line check_line fd in
				let last = Util.find_last_useful_line check_line fd in
				match parse_line first, parse_line last with
				| Some (h, d1), Some (_, d2) -> make ~host: h ~first: d1 ~last: d2 ~file
				| _ -> failwith "From_log.info_of_line")

let compare_info i1 i2 =
	Date.compare i1.first i2.first

let compare_file parse_line f g =
	compare_info (info_of_file parse_line f) (info_of_file parse_line g)

let tail_col_file = 5

let print_all parse_line files =
	let infos = List.map (info_of_file parse_line) files in
	let col f = List.fold_left (fun accu i -> max accu (f i)) 0 infos in
	let col_file = min !To_text.max_task_name_size (col (fun i -> String.length i.file)) in
	let col_host = min !To_text.max_host_name_size (col (fun i -> String.length i.host)) in
	let col_duration = col (fun i -> String.length (Date.Duration.to_string i.duration)) in
	let aux info =
		Printf.printf "%s %s %s %s\n"
			(Util.left_with_tail col_file ~tail: tail_col_file info.file)
			(Util.left col_host info.host)
			(Date.String_of.t info.first)
			(Util.right col_duration (Date.Duration.to_string info.duration))
	in
	List.iter aux (List.sort compare_info infos)
