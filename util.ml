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
(* Author: Vincent Hanquez <vincent.hanquez@citrix.com>         *)
(*         Thomas Gazagnaire <thomas.gazagnaire@citrix.com>     *)

open Printf

exception Cannot_unarchive of string * string
exception Not_unique_dir of string * string

let () = Random.self_init ()

let version = "1.1"
let xenserver_release = "6.0"

(*************************************************************************************)
(*       Imported from stdext to be able to compile outside the chroot               *)
(*************************************************************************************)

(** create a directory but doesn't raise an exception if the directory already exist *)
let mkdir_safe dir perm =
	try Unix.mkdir dir perm with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

(** create a directory, and create parent if doesn't exist *)
let mkdir_rec dir perm =
	let rec p_mkdir dir =
		let p_name = Filename.dirname dir in
		if p_name <> "/" && p_name <> "."
		then p_mkdir p_name;
		mkdir_safe dir perm in
	p_mkdir dir

let spawnvp ?(pid_callback = (fun _ -> ())) cmd args =
	match Unix.fork () with
	| 0 ->
		Unix.execvp cmd args
	| pid ->
		begin try pid_callback pid with _ -> () end;
		snd (Unix.waitpid [] pid)

(** open a file, and make sure the close is always done *)
let with_file file mode perms f =
	let fd = Unix.openfile file mode perms in
	let r =
		try f fd
		with exn -> Unix.close fd; raise exn
	in
	Unix.close fd;
	r

(** Run a function over every line in a file *)
let readfile_line fn fname =
	let fin = open_in fname in
	try
		while true do
			let line = input_line fin in
			fn line
		done;
		close_in fin;
	with
	| End_of_file -> close_in fin
	| exn -> close_in fin; raise exn

(** has_substr str sub returns true if sub is a substring of str. Simple, naive, slow. *)
let has_substr str sub =
	if String.length sub > String.length str then false else
		begin
			let result = ref false in
			for start = 0 to (String.length str) - (String.length sub) do
				if String.sub str start (String.length sub) = sub then result := true
			done;
			!result
		end

(** True if string 'x' ends with suffix 'suffix' *)
let endswith suffix x =
	let x_l = String.length x and suffix_l = String.length suffix in
	suffix_l <= x_l && String.sub x (x_l - suffix_l) suffix_l = suffix

(** True if string 'x' starts with prefix 'prefix' *)
let startswith prefix x =
	let x_l = String.length x and prefix_l = String.length prefix in
	prefix_l <= x_l && String.sub x 0 prefix_l = prefix

(*************************************************************************************)
(*************************************************************************************)

(** bugreport handling **)
let untar_bugreport filename =
	(* weak but we don't care *)
	let random_name () = string_of_int (Random.int 4000000) in
	let rootpwd = "/tmp/logfilter/" ^ (random_name ()) in
	mkdir_rec rootpwd 0o755;
	let opts = if has_substr filename "bz2" then "xjf" else "xzf" in
	let args = [| "tar"; opts; filename; "-C"; rootpwd; |] in
	begin match spawnvp args.(0) args with
		| Unix.WEXITED 0 -> ()
		| Unix.WEXITED i -> raise (Cannot_unarchive (filename, sprintf "exit code %d" i))
		| Unix.WSIGNALED i -> raise (Cannot_unarchive (filename, sprintf "signal code %d" i))
		| _ -> raise (Cannot_unarchive (filename, sprintf "stopped code"))
	end;
	rootpwd

let unzip_bugreport filename =
	(* weak but we don't care *)
	let random_name () = string_of_int (Random.int 4000000) in
	let rootpwd = "/tmp/logfilter/" ^ (random_name ()) in
	mkdir_rec rootpwd 0o755;
	let args = [| "unzip"; "-qq"; filename; "-d"; rootpwd; |] in
	
	begin match spawnvp args.(0) args with
		| Unix.WEXITED 0 -> ()
		| Unix.WEXITED i -> raise (Cannot_unarchive (filename, sprintf "exit code %d" i))
		| Unix.WSIGNALED i -> raise (Cannot_unarchive (filename, sprintf "signal code %d" i))
		| _ -> raise (Cannot_unarchive (filename, sprintf "stopped code"))
	end;
	rootpwd

let find_filename matching prefixes dir =
	let found = ref [] in
	let fqn dir child = dir ^ "/" ^ child in
	let rec visits f dir =
		let childs = ref [] in
		let files =
			try Sys.readdir dir
			with exn -> eprintf "sys readdir: %s\n%!" (Printexc.to_string exn); [||]
		in
		Array.iter (fun name ->
					if Sys.is_directory (fqn dir name) then
						childs := name :: !childs
					else
						f dir name
			) files;
		List.iter (fun child -> visits f (fqn dir child)) (List.rev !childs)
	in
	visits (fun dir file ->
				let matched = ref false in
				List.iter (fun prefix ->
							if matching prefix file then matched := true
					) prefixes;
				if !matched then
					found := (fqn dir file) :: !found
		) dir;
	!found

let find_filename_starting_by = find_filename startswith
let find_filename_ending_by = find_filename endswith
let find_filename_having_substr = find_filename (fun prefix file -> has_substr file prefix)

let pad ?(before = false) n s =
	if String.length s > n then
		(if n > 2 then
				(String.sub s 0 (n - 2))^".."
			else
				String.sub s 0 n)
	else
		let padding = String.make (n - (String.length s)) ' ' in
		if before then padding^s else s^padding

let left n s = pad n s
let right n s = pad n s ~before: true

let left_with_tail col ~tail s =
	let n = String.length s in
	if n <= col
	then left col s
	else Printf.sprintf "%s%s" (left (col - tail) s) (String.sub s (n - tail) tail)

let get_first fd length =
	let (_: int) = Unix.lseek fd 0 Unix.SEEK_SET in
	let buf = String.make length ' ' in
	let (_: int) = Unix.read fd buf 0 length in
	buf

let get_last fd length =
	let (_: int) = Unix.lseek fd (- length) Unix.SEEK_END in
	let buf = String.make length ' ' in
	let (_: int) = Unix.read fd buf 0 (length - 1) in
	buf

let find_useful_line ?(reverse = false) get f fd =
	let chunk_size = 256 in
	let rec aux n =
		let chunk = get fd (n * chunk_size) in
		try
			let line =
				if not (reverse)
				then String.sub chunk 0 (String.index chunk '\n' - 1)
				else let n = String.rindex chunk '\n' in String.sub chunk (n + 1) (chunk_size - n - 1)
			in
			if f line
			then line
			else aux (n + 1)
		with e ->
			aux (n + 1)
	in
	let r = aux 1 in
	let (_: int) = Unix.lseek fd 0 Unix.SEEK_SET in
	r

let find_first_useful_line = find_useful_line get_first
let find_last_useful_line = find_useful_line ~reverse: true get_last

let apply f default = function
	| None -> default
	| Some x -> f x

let may f = function
	| None -> ()
	| Some x -> f x

let regexp_match regexp_string expr =
	let regexp = Str.regexp (regexp_string^"\007") in
	Str.string_match regexp (expr^"\007") 0

let regexp_match_option regexp_string expr_option =
	apply (fun expr -> regexp_match regexp_string expr) false expr_option

let rec iter_with_special_end f g = function
	| [] -> ()
	| [h] -> g h
	| h:: t -> f h; iter_with_special_end f g t

let uniquify table s =
  try Hashtbl.find table s with
      Not_found -> 
        begin
          Hashtbl.add table s s; 
          s
        end

(* size of the terminal columns *)
let size_of_terminal = 76

(* display a tree *)
module Tree =
struct
	let tree = " |_ "
	let final_tree = " \\_ "
	let shift s = s ^ "    "
	let branch_shift s = s ^ " |  "
	
	let tab_of_depth d = 4 * d

	let string_of_node depth tab final name =
		let tab =
			if depth = 0
			then tab
			else if final
			then tab ^ final_tree
			else tab ^ tree
		in
		Printf.sprintf "%s%s" tab name
	
	let dump ~children_of ~name_of ~roots =
		let has_children node = List.length (children_of node) > 1 in
		let rec aux has_brothers depth tab node =
			Printf.printf "%s\n" (string_of_node depth tab (not has_brothers) (name_of node));
			if depth = 0 then
				iter_with_special_end
					(aux (has_children node) 1 tab)
					(aux false 1 tab)
					(children_of node)
			else if has_brothers then
				iter_with_special_end
					(aux (has_children node) (depth + 1) (branch_shift tab))
					(aux false (depth + 1) (branch_shift tab))
					(children_of node)
			else
				iter_with_special_end
					(aux (has_children node) (depth + 1) (shift tab))
					(aux false (depth + 1) (shift tab))
					(children_of node)
		in
		List.iter (aux false 0 "") roots

	let dump_to_html ~children_of ~name_of ~roots =
		let has_children node = List.length (children_of node) >= 1 in
		let rec aux node =
			Printf.printf "%s\n" (name_of node);
			if has_children node then begin
				Printf.printf "<ul>\n";
				List.iter (fun c -> Printf.printf "<li>"; aux c; Printf.printf "</li>\n") (children_of node);
				Printf.printf "</ul>\n"
			end in
		List.iter (fun r -> Printf.printf "<ul><li>"; aux r; Printf.printf "</li></lu>") roots
end
