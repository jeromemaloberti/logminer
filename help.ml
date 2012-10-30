(* Copyright (c) Citrix Systems 2009. All rights reserved.  *)
(* Author: Thomas Gazagnaire <thomas.gazagnaire@citrix.com> *)

open Filter
open String_of.Name

type arg =
	| Reference
	| Uuid
	| Name
	| Rec
	| Duration
	| Int
	| No
	| Trackid
	| Date
	| Thread_id
	| Level

let string_of_arg = function
	| Reference -> Some "reference"
	| Uuid -> Some "uuid"
	| Name -> Some "name"
	| Rec -> Some "(query)"
	| Duration -> Some "compare duration"
	| Int -> Some "compare int"
	| No -> None
	| Trackid -> Some "trackid"
	| Date -> Some "compare date"
	| Thread_id -> Some "thread-id"
	| Level -> Some "debug|warn|info|error"

type t = {
	command : string;
	arg : arg;
	doc : string }

let string_of_t t =
	Printf.sprintf "%s%s%s"
		t.command
		(match string_of_arg t.arg with None -> "" | Some s -> Printf.sprintf " <%s>" s)
		(if t.doc = "" then "" else Printf.sprintf " { %s }" t.doc)

let task_ref = {
	command = task (Task_ref "");
	arg = Reference;
	doc = "" }

let task_uuid = {
	command = task (Task_uuid "");
	arg = Uuid;
	doc = "" }

let task_name = {
	command = task (Task_name "");
	arg = Name;
	doc = "" }

let task_root = {
	command = task (Task_root All);
	arg = Rec;
	doc = "" }

let task_parents = {
	command = task (Task_parents All);
	arg = Rec;
	doc = "" }

let task_children = {
	command = task (Task_children All);
	arg = Rec;
	doc = "" }

let task_lines = {
	command = task (Task_lines (EQ, 0));
	arg = Int;
	doc = "" }

let task_in_database = {
	command = task Task_in_database;
	arg = No;
	doc = "" }

let task_duration = {
	command = task (Task_duration (EQ, Date.Duration.zero));
	arg = Duration;
	doc = "" }

let task_nb_children = {
	command = task (Task_nb_children (EQ, 0));
	arg = Int;
	doc = "" }

let task_not_completed = {
	command = task Task_not_completed;
	arg = No;
	doc = "" }

let task_gap_duration = {
	command = task (Task_gap_duration (EQ, Date.Duration.zero));
	arg = Duration;
	doc = "" }

let session_trackid = {
	command = session (Session_trackid "");
	arg = Trackid;
	doc = "" }

let session_uname = {
	command = session (Session_uname "");
	arg = Name;
	doc = "" }

let session_creation = {
	command = session (Session_creation (Full_date (EQ, Date.zero)));
	arg = Date;
	doc = "" }

let session_destruction = {
	command = session (Session_destruction (Full_date (EQ, Date.zero)));
	arg = Date;
	doc = "" }

let session_duration = {
	command = session (Session_duration (EQ, Date.Duration.zero));
	arg = Duration;
	doc = "" }

let session_nb_tasks = {
	command = session (Session_nb_tasks (EQ, 0));
	arg = Int;
	doc = "" }

let log_date = {
	command = log (Log_date (Full_date (EQ, Date.zero)));
	arg = Date;
	doc = "" }

let log_host = {
	command = log (Log_host "");
	arg = Name;
	doc = "" }

let log_thread_id = {
	command = log (Log_thread_id 0);
	arg = Thread_id;
	doc = "" }

let log_thread_name = {
	command = log (Log_thread_name "");
	arg = Name;
	doc = "" }

let log_error = {
	command = log Log_error;
	arg = No;
	doc = "" }

let log_exception = {
	command = log Log_exception;
	arg = No;
	doc = "" }

let log_level = {
	command = log (Log_level Log.Debug);
	arg = Level;
	doc = "" }

let log_key = {
	command = log (Log_key "");
	arg = Name;
	doc = "" }

let log_gap_duration = {
	command = log (Log_gap_duration (EQ, Date.Duration.zero));
	arg = Duration;
	doc = "" }

let log_msg = {
	command = log (Log_msg "");
	arg = Name;
	doc = "" }

let task_queries =
	"Queries related to tasks:", [
	task_ref;
	task_uuid;
	task_name;
	task_root;
	task_parents;
	task_children;
	task_lines;
	task_in_database;
	task_duration;
	task_nb_children;
	task_not_completed;
	task_gap_duration ]

let session_queries =
	"Queries related to sessions:", [
	session_trackid;
	session_uname;
	session_creation;
	session_destruction;
	session_duration;
	session_nb_tasks ]

let log_queries =
	"Queries related to log lines:", [
	log_date;
	log_host;
	log_thread_id;
	log_thread_name;
	log_error;
	log_exception;
	log_level;
	log_key;
	log_gap_duration;
	log_msg ]

let string_of_queries (header, queries) =
	Printf.sprintf "%s\n%s\n" header (String.concat "\n" (List.map string_of_t queries))

let to_string () : string =
	let headers = [
		"Date are either of the form [HHHHMMDD HH:MM:SS], [HH:MM:SS HHHHMMDD], [HHHHMMDD] or [HH:MM:SS].";
		"Durations are of the form [xxxd][xxxh][xxxm][xxxs] (as in 1d10h).";
		"Comparaison operators are {=,!=,<,<=,>,>= }.";
		"Queries can be composed using usual logical combinator NOT, OR and AND.";
		"Some queries, takes as argument another query, as task-root, which return all the queries whose task roots verify a given query." ] in
	let queries = [log_queries; task_queries; session_queries] in
	Printf.sprintf "%s\n\n%s"
		(String.concat "\n" headers)
		(String.concat "\n" (List.map string_of_queries queries))
