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
(* Author: Jerome Maloberti <jerome.maloberti@citrix.com> *)

%%{
  machine session;
  write data;
}%%

type session = Session of Filter.Base.session | Destruction of string * Date.t

let fail fmt = Printf.ksprintf failwith fmt

let parse_xensource_session date data =
  let cs = ref 0 in
  let p = ref 0 in
  let pe = ref (String.length data) in
  let eof = ref !pe in
  let _pos = ref 0 in
  let pool = ref false in
  let uname = ref "" in
  let local_su = ref false in
  let trackid = ref "" in
(*  let parent_trackid = ref "" in *)
  let auth = ref "" in
  let session_log = ref None in
  let get_current_string () = String.sub data !_pos (!p - !_pos) in

%%{
  action mark_pos { _pos := !p }
  action trackid { trackid := get_current_string () }
  action pool_true { pool := true }
  action uname { uname := get_current_string () }
  action local_su_true { local_su := true }
  action auth { auth := get_current_string () }
#  action parent_trackid { parent_trackid := get_current_string () }
  action session_created {
    session_log := Some (Session (Session.create ~trackid:!trackid ~pool:!pool 
                                    ~uname:!uname ~is_local_superuser:!local_su 
                                    ~auth_user_sid:!auth ~creation:date))
  }
  action session_destroyed {
    session_log := Some (Destruction (!trackid, date))
  }

  trackid = xdigit+ >mark_pos %trackid;
#  parent_trackid = xdigit+ >mark_pos %parent_trackid;
  pool = ('true' @pool_true | 'false');
  uname = 'uname=' . (((any - ' ')+ >mark_pos) %uname)? . space;
  local_su = 'is_local_superuser=' . ('true' @local_su_true | 'false');
  auth = 'auth_user_sid=' . (((any - ' ')+ >mark_pos) %auth)? . space;
  session_created_part := (local_su . space . auth) @session_created  . space ;
  uuid = (xdigit | '-')+;
  opaqueref = 'OpaqueRef:' . uuid;

  session_created = 'create trackid=' . trackid . space . 'pool=' pool . space . 
    uname @{fgoto session_created_part; };
  session_destroyed = ('destroy trackid=' . trackid) %session_destroyed; 
  session_gc = ('destroy _ref=' . opaqueref . space . 'uuid=' . uuid . space .
    'trackid=' . trackid) %session_destroyed . space;
  main := 'Session.' . (session_created | session_destroyed | session_gc);

  write init;
  write exec;
}%%
(*	if !cs < session_first_final then
    fail "session: cs %d < %d" !cs session_first_final; *)
  !session_log
  ;;
(*
let _ =
    let d = "Session.create trackid=64806980b735d0073b13e48fcfc9d628 pool=true uname= is_local_superuser=true auth_user_sid= parent=trackid=9834f5af41c964e225f24279aefe4e49" in
    parse_xensource_session d;
    let d = "Session.destroy trackid=64806980b735d0073b13e48fcfc9d628" in
    parse_xensource_session d;
    let d = "Session.destroy _ref=OpaqueRef:727a7429-a171-66ad-2f31-76d479838ea9 uuid=4d431747-ce99-c923-9d5f-c31451177da2 trackid=ab27c2e6ed2ff78d5f8de081f38985a0 (last active 20121024T09:53:37Z): Timed out session because max number of sessions was exceeded" in
    parse_xensource_session d;;
*)                                     
(*
Session.create trackid=64806980b735d0073b13e48fcfc9d628 pool=true uname= is_local_s
Session.destroy trackid=64806980b735d0073b13e48fcfc9d628

let session_created = Str.regexp "Session.create trackid=\\(.*\\) pool=\\(true\\|false\\) uname=\\(.*\\) is_local_superuser=\\(true\\|false\\) auth_user_sid=\\(.*\\)"

Sep 24 02:58:56 localhost xapi: [ info|x6|7 UNIX /var/xapi/xapi|session.slave_login D:41531dc724f8|xapi] Session.create trackid=64806980b735d0073b13e48fcfc9d628 pool=true uname= is_local_superuser=true auth_user_sid= parent=trackid=9834f5af41c964e225f24279aefe4e49
Oct 24 11:27:07 localhost xapi: [debug|dt34|22 db_gc|DB GC D:783a94904545|db_gc] Session.destroy _ref=OpaqueRef:727a7429-a171-66ad-2f31-76d479838ea9 uuid=4d431747-ce99-c923-9d5f-c31451177da2 trackid=ab27c2e6ed2ff78d5f8de081f38985a0 (last active 20121024T09:53:37Z): Timed out session because max number of sessions was exceeded

*)
