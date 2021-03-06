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
(* Author: Jerome Maloberti <jerome.maloberti@citrix.com>     *)

let load_file ?(filter=Filter.All) filename = From_log.to_db filter [filename]

let load_db ?(filter=Filter.All) filename = Db.from_backup_file filter filename

let save_db db filename = Db.to_backup_file db filename

let head ?(n=10) l =
  let rec aux n l' acc =
    if n = 0 then acc
    else
      match l' with
        | [] -> acc
        | hd :: tl -> aux (n-1) tl (hd :: acc)
  in 
  let res = aux n l [] in
  List.rev res

let tail ?(n=10) l =
  let res = head ~n (List.rev l) in
  List.rev res

let last_logs ?(n=10) db = tail ~n (db.Db.logs)

let last_tasks ?(n=10) db = tail ~n (db.Db.tasks)

let last_sessions ?(n=10) db = tail ~n (db.Db.sessions)

let first_logs ?(n=10) db = head ~n (db.Db.logs)

let first_tasks ?(n=10) db = head ~n (db.Db.tasks)

let first_sessions ?(n=10) db = head ~n (db.Db.sessions)

let print_logs l = To_text.Log.from_log_list l

let print_tasks l = To_text.Task.from_task_list l

let print_sessions l = To_text.Session.from_session_list l

let filter_create f =
  let f = Parser.main Lexer.main (Lexing.from_string (f ^ ";")) in
	Printf.printf "The current filter is %s.\n%!" (Filter.String_of.t f);
	f

let filter_db f db = Db.filter f db

module StringSet = Set.Make(String)

let list_keys db =
  let set = 
    let logs = db.Db.logs in
    List.fold_left (fun s l -> StringSet.add l.Log.key s) StringSet.empty logs in 
  StringSet.elements set

let count_logs db =
  let table = Hashtbl.create 75000 in
  let counter = ref 1 in
  let count word =
    begin
      if (!counter mod 100) = 0 then Printf.printf "counting logs %d hash %d\n" !counter (Hashtbl.length table);
      try incr (Hashtbl.find table word)
      with Not_found -> Hashtbl.add table word (ref 1)
    end in
  let logs = db.Db.logs in
  List.iter (fun l -> count (Queue.top l.Log.msg)) logs;
  let a = Array.create (Hashtbl.length table) ("",ref 0) in
  let i = ref 0 in
  Hashtbl.iter (fun k v -> a.(!i) <- (k,v); incr i) table;
  let compare_fun (k1,v1) (k2,v2) =
    let res = Util.compare_int !v2 !v1 in
    if res <> 0 then res 
    else String.compare k1 k2 in
  Array.sort compare_fun a;
  a

let match_regexp r s = 
  try 
    ignore(Str.search_forward r s 0);
    true
  with Not_found ->
    false

let fold_root_tasks f acc tasks =
  let test acc task = if Task.parent task <> None then
      acc else (f acc task) in
  let l = List.fold_left test acc tasks in
  List.rev l

let tasks_summary ?(filter) tasks =
  let regexp_ignore = 
    Str.regexp "^xapi events\\|^dispatch:.*\\|^xenops events\\|^session.log*\\|^session.slave*" in
  let task_summary t =
    (t.Task.thread_id, t.Task.task_ref, (Date.String_of.time (Date.time t.Task.creation)), 
     (Date.Duration.to_string (Task.duration t)), t.Task.task_name) in
  let add acc task = 
    if (match_regexp regexp_ignore task.Task.task_name) then
      acc
    else 
      match filter with None -> (task_summary task) :: acc
      | Some f -> if (match_regexp f task.Task.task_name) then
	  (task_summary task) :: acc
	else
	  acc
  in
  fold_root_tasks add [] tasks


let tasks_timings tasks =
  let regexp_ignore = 
    Str.regexp "^xapi events\\|^dispatch:.*\\|^xenops events\\|^session.log*\\|^session.slave*" in
  let gather_logs task =
    let acc = ref [] in
    let rec aux t =
      acc := List.append !acc t.Task.logs;
      List.iter (fun c -> aux c) t.Task.children in
    aux task;
    !acc in
  let search_logs regexp task logs s =
    let r = Str.regexp regexp in
    let res = List.filter (fun l -> match_regexp r (Log.msg l)) logs in
    List.map (fun l -> (Date.Duration.to_float (Date.sub l.Log.date task.Task.creation)),
      (Date.to_float l.Log.date), s(*,(Log.msg l)*)) res in
  let task_summary t =
    let logs = gather_logs t in
    (t.Task.task_ref, (Date.to_float t.Task.creation),
     (Date.Duration.to_float (Task.duration t)), 
     (Date.Duration.to_float (t.Task.total_stats.Task.gap)),
   (*  (search_logs "^Device.Dm.start.*" t logs "Device.Dm.start"), *)
     (search_logs "^VM.unpause" t logs "VM.unpause"),
     (search_logs "^UPDATES.refresh_vm" t logs "refresh_vm"),
     (search_logs "^UPDATES.inject_barrier" t logs "inject_barrier"),
     (search_logs "^Signalling xenapi event thread to re-register" t
     logs "re-register"),
     (search_logs "^VM.stat " t logs "VM.stat")
) in
  let add acc task = 
    if (match_regexp regexp_ignore task.Task.task_name) then
      acc
    else 
      (task_summary task) :: acc
  in
  fold_root_tasks add [] tasks


