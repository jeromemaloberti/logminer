(* Copyright (c) Citrix Systems 2008-2009. All rights reserved. *)
(* Author: Thomas Gazagnaire <thomas.gazagnaire@citrix.com>     *)

(*************************************************************************)
(* dates                                                                 *)
(*************************************************************************)
open Printf

type date = { day: int; month: int; year: int }
type time = { hours: int; minutes: int; seconds: int; ms: int }
type t = { date: date; time: time; mutable line: int }
type duration = t

let make_date ~year ~month ~day =
	{ day = day; month = month; year = year }

let make_time ~hours ~minutes ~seconds ~ms =
	{ hours = hours; minutes = minutes; seconds = seconds; ms = ms }

let make ~year ~month ~day ~hours ~minutes ~seconds ~ms ~line =
	{ date = make_date ~year ~month ~day; time = make_time ~hours ~minutes~ seconds ~ms; line = line }

let make2 ~time ~date =
	{ date = date; time = time; line = 0 }

let date d = d.date
let time d = d.time

let year d = d.date.year
let month d = d.date.month
let day d = d.date.day
let hours d = d.time.hours
let minutes d = d.time.minutes
let seconds d = d.time.seconds
let ms d = d.time.ms

let zero : t = make 0 0 0 0 0 0 0 0
let max_date : t = make max_int max_int max_int max_int max_int max_int max_int max_int

let to_float fd =
	let tm =
		{ Unix.tm_year = fd.date.year - 1900;
		  Unix.tm_mon = fd.date.month;
		  Unix.tm_mday = fd.date.day;
		  Unix.tm_hour = fd.time.hours;
		  Unix.tm_min = fd.time.minutes;
		  Unix.tm_sec = fd.time.seconds;
		  Unix.tm_wday = 0; Unix.tm_yday = 0; Unix.tm_isdst = true }
	in fst (Unix.mktime tm) +. (float_of_int fd.time.ms) /. 1000.

let sub a b : duration =
	if a = max_date
	then max_date
	else if b = max_date
	then zero
	else begin
		let sub_seconds = (to_float a) -. (to_float b) +. 0.001 in
		let sub = Unix.gmtime sub_seconds in
		make
			~year: (sub.Unix.tm_year - 70) ~month: sub.Unix.tm_mon ~day: (sub.Unix.tm_mday - 1 ) ~line: 0
			~hours: sub.Unix.tm_hour ~minutes: sub.Unix.tm_min ~seconds: sub.Unix.tm_sec ~ms: ((1000 + a.time.ms - b.time.ms) mod 1000)
	end

let compare_date a b =
	if compare a.year b.year <> 0
	then compare a.year b.year
	else if compare a.month b.month <> 0
	then compare a.month b.month
	else compare a.day b.day

let compare_time a b =
	if compare a.hours b.hours <> 0
	then compare a.hours b.hours
	else if compare a.minutes b.minutes <> 0
	then compare a.minutes b.minutes
	else if compare a.seconds b.seconds <> 0
	then compare a.seconds b.seconds
	else compare a.ms b.ms

let compare a b =
	if compare_date a.date b.date <> 0
	then compare_date a.date b.date
	else if compare_time a.time b.time <> 0
	then compare_time a.time b.time
	else compare a.line b.line

let sort a b =
	if compare a b > 0
	then (b, a)
	else (a, b)

let min a b = fst (sort a b)
let max a b = snd (sort a b)

module String_of =
struct
	let date d =
		sprintf "%04i%02i%02i" d.year d.month d.day
	
	let time ?(verbose = false) t =
		sprintf "%02i:%02i:%02i%s" t.hours t.minutes t.seconds (if verbose then sprintf ".%03i" t.ms else "")
	
	let t ?(sep = " ") ?(verbose = false) d =
		if d = max_date
		then "++"
		else sprintf "%s%s%s" (date d.date) sep (time ~verbose d.time)
end

module Duration =
struct
	type t = duration
	
	let to_float d =
		(float_of_int d.time.ms) /. 1000.
		+. float_of_int d.time.seconds
		+. (float_of_int d.time.minutes) *. 60.
		+. (float_of_int d.time.hours) *. 3600.
		+. (float_of_int d.date.day) *. 86400.

	let of_string d =
		let r = Str.regexp "\\([0-9]+d\\)?\\([0-9]+h\\)?\\([0-9]+m\\)?\\([0-9.]+s\\)?" in
		if Str.string_match r d 0
		then begin
			let num n = let s = Str.matched_group n d in String.sub s 0 (String.length s - 1) in
			let aux n = try int_of_string (num n) with Not_found -> 0 in
			let ms, s = try modf (float_of_string (num 4)) with Not_found -> 0., 0. in
			{ date = {
					year = 0;
					month = 0;
					day = aux 1 };
				time = {
					hours = aux 2;
					minutes = aux 3;
					seconds = int_of_float s;
					ms = int_of_float (ms *. 1000.) };
				line = 0;
			}
		end else begin
			Printf.eprintf "error while converting duration\n%!";
			exit 5
		end
	
	let max = max
	let min = min
	let zero = zero
	let max_duration = max_date
	let compare = compare
	
	let to_string ?(verbose = false) d =
		let aux num unit =
			if num = 0
			then ""
			else sprintf "%i%s" num unit
		and aux_sec seconds ms =
			if seconds = 0 && ms = 0
			then sprintf ""
			else if seconds = 0
			then sprintf "0.%03is" ms
			else if ms = 0
			then sprintf "%is" seconds
			else sprintf "%i.%is" seconds ms
		in
		if d = max_date
		then "++"
		else if d = zero
		then "0s"
		else sprintf "%s%s%s%s%s%s"
				(aux (year d) "y")
				(aux (month d) "m")
				(aux (day d) "d")
				(aux (hours d) "h")
				(aux (minutes d) "m")
				(if not verbose && (hours d <> 0 || day d <> 0 || month d <> 0 || year d <> 0)
					then ""
					else aux_sec (seconds d) (if not verbose && minutes d = 0 && seconds d = 0 then ms d else 0))
end
