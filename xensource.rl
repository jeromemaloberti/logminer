let quiet = ref false

let info (fmt: ('a, unit, string, unit) format4) : 'a =
	Printf.kprintf (fun s -> if not !quiet then print_string s; flush stdout) fmt

let debug (fmt: ('a, unit, string, unit) format4) : 'a =
	Printf.kprintf (fun s -> print_endline s; flush stdout) fmt

let fail fmt = Printf.ksprintf failwith fmt

let cache_task_names = Hashtbl.create 128
let cache_thread_infos = Hashtbl.create 16
let cache_hosts = Hashtbl.create 16
let cache_keys = Hashtbl.create 32

exception Goto_out

let line_num = ref 0

type task = TaskDestruction 
            | TaskCreation of string * string * string option * string option * string option * bool
            | TaskAsync of string option
            | Nothing

type session = Session of Filter.Base.session | Destruction of string * Date.t

%%{
  machine xensource;
  write data;
  include date_time "date_time.rl";
  include message "message.rl";
}%%

let parse_xensource ?log_counter data =
  let cs = ref 0 in
  let p = ref 0 in
  let pe = ref (String.length data) in
  let eof = ref !pe in
  let month = ref 0 in
  let day = ref 0 in
  let hour = ref 0 in
  let min = ref 0 in
  let sec = ref 0 in
  let thread_id = ref 0 in
  let thread_info = ref None in
  let level = ref Log.Debug in
  let host = ref "" in
  let empty_log_line = ref false in 
  let task_name = ref None in
  let task_ref = ref None in
  let task_ref_bool = ref false in
  let key = ref "" in
  let message = ref "" in
  let error = ref Log.Ok in
  let _int = ref 0 in 
  let _pos = ref 0 in
  let get_current_string () = String.sub data !_pos (!p - !_pos) in
  
  let pool = ref false in
  let uname = ref "" in
  let local_su = ref false in
  let trackid = ref "" in
(*  let parent_trackid = ref "" in *)
  let auth = ref "" in
  let session_log = ref None in
  let date = ref None in
  let mesg_task_name = ref "" in
  let mesg_task_ref = ref "" in
  let mesg_task_uuid = ref None in
  let mesg_task_trackid = ref None in
  let mesg_parent_task_ref = ref None in
  let mesg_task = ref Nothing in
%%{
  action do_nothing { }
  action print_string { Printf.printf "[%s] " (get_current_string ()) }
  action thread_id { 
    thread_id := !_int;
    date := Some (Date.make ~day:!day ~month:!month ~year:2012 ~hours:!hour ~minutes:!min ~seconds:!sec ~ms:0 ~line: (match log_counter with Some l -> l | None -> 0))
  }
  action thread_info { thread_info := Some (Util.uniquify cache_thread_infos (get_current_string ())) }
  action info { level := Log.Info }
  action warn { level := Log.Warn }
  action debug { level := Log.Debug }
  action error { level := Log.Error }
  action host { host := Util.uniquify cache_hosts (get_current_string ()) }
  action key { key := Util.uniquify cache_keys (get_current_string ()) }
  action repeat_msg { empty_log_line := true; fbreak; }
  action task_name { 
    if !task_ref_bool then begin
      task_ref := Some (String.sub data (!p - 14) 14);
      task_name := Some (Util.uniquify cache_task_names (String.sub data !_pos (!p - !_pos - 15)));
    end else begin
      task_name := Some (Util.uniquify cache_task_names (get_current_string ()))
    end
  }
  action task_ref { task_ref_bool := true; }

  host1 = (any - space)+;
  host2 = (any - '|')+ >mark_pos %host;
  path = '/' ((any - (space | '/'))+ '/')*; 
  program = path? . ((any - (space | ':'))+ >mark_pos) %do_nothing . ':';
  level_re = (' info' @info | 'debug' @debug | 'error' @error | ' warn' @warn);
  thread_id_re = (digit+ >clear_int $add_int) @thread_id ;
  thread_info_re = (any - '|')+ >mark_pos %thread_info;
  thread = thread_id_re (space thread_info_re)?;
  task_ref = (('D'|'R') ':' xdigit{12}) >task_ref;
  task_name = (any - ('|'))+ ;
  task = ((task_name >mark_pos :> task_ref) %task_name) | ((task_name >mark_pos) %task_name) | "";
  key = (any - ']')+ >mark_pos %key;
  description = '[' level_re '|' host2 '|' thread '|' task '|' key ']';
  squeezed = path? . 'squeezed:'; 
  repeat_msg = ('last message repeated') @repeat_msg;
  main := date_time_re . space . host1 . space . 
    ((program . space . description . space . message) | repeat_msg);

  write init;
}%%
  begin
    try 
%%{
    write exec;
}%%
    with Goto_out -> cs := xensource_first_final | _ -> ();
  end;

  line_num := !line_num + 1;
	if !cs < xensource_first_final then
    fail "xensource: line %d cs %d < %d" !line_num !cs xensource_first_final;

  if !empty_log_line then begin
    (None,None,Nothing)
  end else begin
    let d = match !date with None -> fail "Date not created"
      | Some v -> v in
    let log = (d, !host, !level, !thread_info, !thread_id, !task_ref, !task_name, !key, !error, !message) in
    (Some log, !session_log, !mesg_task)
  end
  ;;
(*
    390 string [squeezed]
     22 string [v6d]
  62613 string [xapi]
    713 string [xcp-networkd]
   1899 string [xcp-rrdd]
   5670 string [xenopsd]
*)

