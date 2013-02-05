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

(* Online filtering means that we can select at parsing time if a log line    *)
(* have to be kept or not. By default, we keep all the logs line, because     *)
(* the order in which we parse xensource.log files is not always relevant and *)
(* that most of the task filters need to have global information.             *)
module Online =
struct
	let item = function
		| Filter.Task _ | Filter.Session _ -> false
		| _ -> true
	
	let rec t = function
		| Filter.Item i -> item i
		| Filter.Or l | Filter.And l -> List.for_all t l
		| Filter.Not n -> t n
		| Filter.All -> false
end

module Number_of_date =
struct
	let task accu _ = accu
	
	let date accu = function
		| Filter.Full_date _ | Filter.Date _ -> accu + 1
		| _ -> accu
	
	let log accu = function
		| Filter.Log_date d -> date accu d
		| _ -> accu
	
	let item accu = function
		| Filter.Log l -> log accu l
		| _ -> accu
	
	let rec t accu = function
		| Filter.Item i -> item accu i
		| Filter.Or l | Filter.And l -> List.fold_left t accu l
		| Filter.Not n -> t accu n
		| Filter.All -> accu
end

(* Monotonous filters are filters for which it is sufficient to read the first and last *)
(* lines of a file to know if it is worth reading the contents of that file.            *)
module Monotonous =
struct
	let task _ = false
	
	let date = function
		| Filter.Full_date _ | Filter.Date _ -> true
		| Filter.Time _ -> false
	
	let log = function
		| Filter.Log_date d -> date d
		| Filter.Log_host _ -> true
		| _ -> false
	
	let item = function
		| Filter.Task t -> task t
		| Filter.Log l -> log l
		| _ -> false
	
	let rec t = function
		| Filter.Item i -> item i
		| Filter.Or l | Filter.And l -> List.fold_left Number_of_date.t 0 l <= 1 && List.for_all t l
		| Filter.Not n -> t n
		| Filter.All -> false
end

let can_filter_online = Online.t
let is_monotonous = Monotonous.t
