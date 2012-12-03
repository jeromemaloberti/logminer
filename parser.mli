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

type token =
  | END
  | LE
  | LEQ
  | GE
  | GEQ
  | EQ
  | NE
  | RPAR
  | LPAR
  | OR
  | AND
  | INT of (int)
  | STRING of (string)
  | REF of (string)
  | DATEFMT of (string)
  | TIMEFMT of (string)
  | TIMEMSFMT of (string)
  | DURATIONFMT of (string)
  | THREAD_ID
  | THREAD_NAME
  | TASK_REF
  | TASK_NAME
  | HOST
  | LEVEL
  | KEY
  | TIME
  | DATE
  | ALL
  | NOT
  | MSG
  | TASK_ROOT
  | TASK_PARENTS
  | TASK_CHILDREN
  | TASK_EXCEPTIONS
  | TASK_ERRORS
  | TASK_IN_DB
  | TASK_DURATION
  | TASK_LINES
  | TASK_NB_CHILDREN
  | TASK_UUID
  | GAP_DURATION
  | TASK_GAP_DURATION
  | TASK_NOT_COMPLETED
  | SESSION_TRACKID
  | SESSION_UNAME
  | SESSION_CREATION
  | SESSION_DESTRUCTION
  | SESSION_DURATION
  | SESSION_NB_TASKS

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Filter.t
