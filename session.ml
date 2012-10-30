(* Copyright (c) Citrix Systems 2009. All rights reserved.  *)
(* Author: Thomas Gazagnaire <thomas.gazagnaire@citrix.com> *)

(* Management of sessions. *)

(* 'a is the task type *)
type 'a t = {
	trackid: string;
	pool: bool;
	uname: string;
	is_local_superuser: bool;
	auth_user_sid: string;
	creation: Date.t;
	mutable destruction: Date.t option;
	mutable tasks : 'a list }

let create ~trackid ~pool ~uname ~is_local_superuser ~auth_user_sid ~creation = {
	trackid = trackid;
	pool = pool;
	uname = uname;
	is_local_superuser = is_local_superuser;
	auth_user_sid = auth_user_sid;
	creation = creation;
	destruction = None;
	tasks = [] }

let uname s = s.uname
let trackid s = s.trackid
let is_local_superuser s = s.is_local_superuser
let auth_user_sid s = s.auth_user_sid

let creation s = s.creation

let destruction s =
	match s.destruction with
	| Some d -> d
	| None -> Date.max_date

let set_destruction s d = s.destruction <- Some d

let duration s =
	match s.destruction with
	| Some d -> Date.sub d s.creation
	| _ -> Date.Duration.max_duration

let is_destroyed s = s.destruction <> None

let tasks s = s.tasks
let add_task s t = s.tasks <- t :: s.tasks

let compare a b =
	Date.compare a.creation b.creation
