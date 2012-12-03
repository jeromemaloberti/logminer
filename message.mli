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

type task = TaskDestruction 
            | TaskCreation of string * string * string option * string option * string option * bool
            | TaskAsync of string option
            | Nothing

type session = Session of Filter.Base.session | Destruction of string * Date.t

val parse_message : string -> Date.t -> (Log.error * session option * task)
