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
(* Author: Thomas Gazagnaire <thomas.gazagnaire@citrix.com>     *)

(* levels *)
type level = Debug | Warn | Info | Error

let level_of_string = function
	| "debug" -> Debug
	| "warn" | " warn" -> Warn
	| "info" | " info" -> Info
	| "error" -> Error
	| s -> failwith (Printf.sprintf "Types.level_of_string: '%s' is not a level" s)

let string_of_level = function
	| Debug -> "debug"
	| Warn -> "warn"
	| Info -> "info"
	| Error -> "error"

(* errors *)
type error = Ok | Got_exception | Got_error

(* logs *)
(* 'a is task type *)
type 'a t = {
	date: Date.t;
	level: level;
	host: string;
	thread_name: string option;
	thread_id: int;
	task_ref: string option;
	task_name: string option;
	mutable tasks: 'a list;
	key: string;
	error: error;
	mutable gap: Date.Duration.t;
	mutable msg: string Queue.t }

let make ~date ~host ~level ~thread_name ~thread_id ~task_ref ~task_name ~key ~error ~msg =
	let q_msg = Queue.create () in
	Queue.add msg q_msg;
	{
		date = date;
		host = host;
		level = level;
		thread_name = thread_name;
		thread_id = thread_id;
		task_ref = task_ref;
		task_name = task_name;
		key = key;
		error = error;
		tasks = [];
		msg = q_msg;
		gap = Date.Duration.zero;
	}

let sort a b =
  if Date.compare a.date b.date > 0 
  then (b, a)
  else (a, b)

let min a b = fst (sort a b)
let max a b = snd (sort a b)

let task_ref l = l.task_ref
let task_name l = l.task_name
let host l = l.host
let thread_id l = l.thread_id
let thread_name l = l.thread_name
let date l = l.date
let level l = l.level
let key l = l.key
let error l = l.error
let gap l = l.gap

let msg l =
	let accu = ref [] in
	Queue.iter (fun msg -> accu := msg :: !accu) l.msg;
	accu := List.rev !accu;
	String.concat "\n" !accu

let tasks l = l.tasks
let add_task l t = l.tasks <- t :: l.tasks

let rec compute_gap logs =
	let rec aux accu = function
		| [] | [_] -> accu
		| h1 :: h2 :: t -> aux (Date.Duration.max accu (Date.sub h2.date h1.date)) (h2 :: t)
	in
	aux Date.Duration.zero logs

let compare a b =
	Date.compare a.date b.date
