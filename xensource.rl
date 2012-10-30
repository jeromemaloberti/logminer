let quiet = ref false

let info (fmt: ('a, unit, string, unit) format4) : 'a =
	Printf.kprintf (fun s -> if not !quiet then print_string s; flush stdout) fmt

let debug (fmt: ('a, unit, string, unit) format4) : 'a =
	Printf.kprintf (fun s -> print_endline s; flush stdout) fmt

let fail fmt = Printf.ksprintf failwith fmt

%%{
  machine xensource;
  write data;
  include date_time "date_time.rl";
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
  
%%{
  action do_nothing { }
  action mark_pos { _pos := !p }
  action print_string { Printf.printf "[%s] " (get_current_string ()) }
  action thread_id { thread_id := !_int }
  action thread_info { thread_info := Some (get_current_string ()) }
  action info { level := Log.Info }
  action warn { level := Log.Warn }
  action debug { level := Log.Debug }
  action error { level := Log.Error }
  action host { host := get_current_string () }
  action key { key := get_current_string () }
  action message { message := get_current_string () }
  action repeat_msg { empty_log_line := true }
  action task_name { 
    if !task_ref_bool then begin
      task_ref := Some (String.sub data (!p - 14) 14);
      task_name := Some (String.sub data !_pos (!p - !_pos - 15));
    end else begin
      task_name := Some (get_current_string ())
    end
  }
  action task_ref { task_ref_bool := true; }
  action is_exception { 
    error := Log.Got_exception; 
    _pos := !p;
  }
  action is_error { 
    error := Log.Got_error; 
  }

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
  is_exception = 'Raised at ' >is_exception . any*;
  is_error = any* >mark_pos :> 'Got exception' >is_error . any*; 
  is_internal = any* >mark_pos :> 'INTERNAL' >is_error . any*; 
  message = ((any* >mark_pos) | (is_exception) | (is_error) | (is_internal)) %/message;
  squeezed = path? . 'squeezed:'; 
  repeat_msg = ('last message repeated ' . any+) @repeat_msg;
  main := date_time_re . space . host1 . space . 
    ((program . space . description . space . message) | repeat_msg);

  write init;
  write exec;
}%%

	if !cs < xensource_first_final then
    fail "xensource: cs %d < %d" !cs xensource_first_final;
(*
  let get_string v = match v with None -> "(none)" | Some s -> s in
  if !empty_log_line then begin
    Printf.printf "Empty\n"
  end else begin
    Printf.printf " TID %d name %s level %s host %s task [%s] ref [%s] key [%s] msg [%s]\n" 
      !thread_id (get_string !thread_info) 
      (Log.string_of_level !level) !host (get_string !task_name) (get_string !task_ref)
      !key !message
  end;
*)
  if !empty_log_line then begin
    None
  end else begin
    let date = Date.make ~day:!day ~month:!month ~year:2012 ~hours:!hour ~minutes:!min ~seconds:!sec ~ms:0 ~line: (match log_counter with Some l -> l | None -> 0) in
    let log = (date, !host, !level, !thread_info, !thread_id, !task_ref, !task_name, !key, !error, !message) in
    Some log
  end
  ;;
(*  
let () =
  let date = "Sep 24 02:58:30 localhost /opt/xcp-networkd: [debug|localhost.localdomain|0 thread_zero|Task blah R:001122334455|networkd] Error whilst importing db objects into master; aborted: Api_errors.Server_error(INTERNAL_ERROR, _)" in
  let date2 = "Sep 24 02:58:54 localhost last message repeated 2 times" in
  ignore(parse_xensource date); 
  ignore(parse_xensource date2);
  let date = "Sep 24 02:58:30 localhost /opt/xcp-networkd: [debug|localhost.localdomain|0 thread_zero|Task blah|networkd] Xapi_cli.exception_handler: Got exception INTERNAL_ERROR: [ Cli_util.Cli_failure(Key iscsi_iqn not found in map) ]" in
  ignore(parse_xensource date); 
  let date = "Sep 24 02:58:30 localhost /opt/xcp-networkd: [debug|localhost.localdomain|0 thread_zero||networkd] Raised at client.ml:6.37-75 -> xapi_xenops.ml:1398.19-89 -> pervasiveext.ml:22.2-9" in
  ignore(parse_xensource date); 
  let ic = open_in Sys.argv.(1) in
  try
    while true do
      ignore(parse_xensource (input_line ic))
    done
  with End_of_file ->
    close_in ic 
;;
*)
(*
    390 string [squeezed]
     22 string [v6d]
  62613 string [xapi]
    713 string [xcp-networkd]
   1899 string [xcp-rrdd]
   5670 string [xenopsd]
*)

