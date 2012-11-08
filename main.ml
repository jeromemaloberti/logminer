(* Copyright (c) Citrix Systems 2008-2009. All rights reserved. *)
(* Author: Thomas Gazagnaire <thomas.gazagnaire@citrix.com>     *)

open Printf

type format = Text | Jira | CSV | HTML | Stats
type display = Log | Task | Session | Info | Help

let _ =
	let save_file = ref "" in
	let load_file = ref "" in
	let filter_string = ref "" in
	let files = ref [] in
	let display = ref Log in
	let format = ref Text in
	
	let set_format x () =
		match !format with
		| Jira | CSV | HTML | Stats -> failwith "Please select only one display format"
		| Text -> format := x
	in
	
	let set_display x () = match !display with
		| Log -> display := x
		| Task | Session | Info | Help -> failwith "conflict between '-i' '-t' '-s' ' options : please pick only one of them in the command line" in
	
	let print_version () =
		printf "LogFilter, version %s\nCopyright (c) Citrix Systems 2008-2009. All rights reserved.\nThomas Gazagnaire <thomas.gazagnaire@citrix.com>\n" Util.version;
		exit 0
	in
	
	Arg.parse
		(Arg.align [
				"--version", Arg.Unit print_version, " Display the version number and exit.";
				"--help-query", Arg.Unit (set_display Help), " Display the query language help";
				"-f", Arg.Set_string filter_string, " Query expression to filter either a log file or a task tree -- see README file.";
				"-t", Arg.Unit (set_display Task), " Display the task tree of the filtered log lines.";
				"-s", Arg.Unit (set_display Session), " Display the session list of the filtered log lines.";
				"-i", Arg.Unit (set_display Info), " Display the timeline information of the provided log files.";
				"--save-db", Arg.Set_string save_file, " Save the internal database to the given file.";
				"--load-db", Arg.Set_string load_file, " Load an internal database saved in the given file.";
				"--jira", Arg.Unit (set_format Jira), " Set the output format to JIRA markup language.";
				"--csv", Arg.Unit (set_format CSV), " Set the output format to CSV (Comma Separated Values).";
				"--html", Arg.Unit (set_format HTML), " Set the output format to HTML.";
				"--stats", Arg.Unit (set_format Stats), " Set the output format to gnuplot.";
				"--force-monotonous", Arg.Set From_log.force_monotonous, " Force the filtering query to be monotonous -- speed-up the parsing of log files.";
				"--quiet", Arg.Set From_log.quiet, " Do not display any information message.";
				"-v", Arg.Unit (To_text.verbose 1), " Display more information -- task tree display only.";
				"-vv", Arg.Unit (To_text.verbose 2), " Display more information -- task tree display only.";
				"--max-task-name-width", Arg.Set_int To_text.max_task_name_size, " Set the maximun width of the task name column -- task tree display only.";
				"--max-host-name-width", Arg.Set_int To_text.max_host_name_size, " Set the maximun width of the host name column -- task tree display only.";
				"--no-auto-resize", Arg.Clear To_text.auto_resize, " Do not auto-resize the column widths -- speed-up the task tree and session list displays.";
				])
		(fun s -> files := s :: !files)
		(sprintf "usage: %s [-f query] [-t|-s] log1 [log2...] (logN is either a xensource.log file, a bug-report archive or a directory)" Sys.argv.(0));
	
	if !files = [] && !load_file = "" && !display <> Help then begin
		eprintf "Please specify a list of files to parse or use the --load-db <filename> option\n";
		exit (- 1);
	end;
	
	match !display with
	| Info ->
		Info.print_all From_log.info_of_line (List.flatten (List.map From_log.get_all_files !files));
		exit 0
	| Help ->
		Printf.printf "%s" (Help.to_string ());
		exit 0
	| display ->
	(* 1. Check if online filter is possible*)
		let filter =
			if !filter_string = "" then
				Filter.All
			else try
					let f = Parser.main Lexer.main (Lexing.from_string (!filter_string ^ ";")) in
					printf "The current filter is %s.\n%!" (Filter.String_of.t f);
					f
				with _ ->
					eprintf "Error: bad query format. The query can be constructred from:\n%s" (Help.to_string ());
					exit 1 in
		
		(* 2. Parse the logs files to build the database, or unmarshal an existing one *)
		let db =
			if !load_file <> "" then
				Db.from_backup_file filter !load_file
			else
				From_log.to_db filter !files
		in
		
		(* 3. Save the database if necessary *)
		if !save_file <> "" then begin
			Db.to_backup_file db !save_file;
			exit 0;
		end;
		
		(* 4. Finally, display either one aggregated log file or a task tree *)
		(match !format, display with
		| Text, Log -> To_text.Log.from_db db
		| Text, Task -> To_text.Task.from_db db
		| Text, Session -> To_text.Session.from_db db
		
		| Jira, Log -> To_jira.Log.from_db db
		| Jira, Task -> To_jira.Task.from_db db
		| Jira, Session -> failwith "not implemented"
		
		| CSV, Log -> To_csv.Log.from_db db
		| CSV, Task -> To_csv.Task.from_db db
		| CSV, Session -> failwith "not implemented"
		
		| HTML, Log -> To_html.Log.from_db db
		| HTML, Task -> To_html.Task.from_db db
		| HTML, Session -> To_html.Session.from_db db

		| Stats, Log -> To_stats.Log.from_db db
		| Stats, Task -> To_stats.Task.from_db db
		| Stats, Session -> To_stats.Session.from_db db

		| _, _ -> assert false);
    Gc.print_stat stdout;;
