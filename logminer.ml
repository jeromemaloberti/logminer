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
  let compare (k1,v1) (k2,v2) =
    let res = !v2 - !v1 in
    if res <> 0 then res 
    else String.compare k1 k2 in
  Array.sort compare a;
  a
