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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
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

let parse_error s = (* Called by the parser function on error *)
    Printf.eprintf "query: %s on characters %i-%i\n" s (symbol_start ()) (symbol_end ());
    flush stdout;
    exit 4
# 75 "parser.ml"
let yytransl_const = [|
  257 (* END *);
  258 (* LE *);
  259 (* LEQ *);
  260 (* GE *);
  261 (* GEQ *);
  262 (* EQ *);
  263 (* NE *);
  264 (* RPAR *);
  265 (* LPAR *);
  266 (* OR *);
  267 (* AND *);
  275 (* THREAD_ID *);
  276 (* THREAD_NAME *);
  277 (* TASK_REF *);
  278 (* TASK_NAME *);
  279 (* HOST *);
  280 (* LEVEL *);
  281 (* KEY *);
  282 (* TIME *);
  283 (* DATE *);
  284 (* ALL *);
  285 (* NOT *);
  286 (* MSG *);
  287 (* TASK_ROOT *);
  288 (* TASK_PARENTS *);
  289 (* TASK_CHILDREN *);
  290 (* TASK_EXCEPTIONS *);
  291 (* TASK_ERRORS *);
  292 (* TASK_IN_DB *);
  293 (* TASK_DURATION *);
  294 (* TASK_LINES *);
  295 (* TASK_NB_CHILDREN *);
  296 (* TASK_UUID *);
  297 (* GAP_DURATION *);
  298 (* TASK_GAP_DURATION *);
  299 (* TASK_NOT_COMPLETED *);
  300 (* SESSION_TRACKID *);
  301 (* SESSION_UNAME *);
  302 (* SESSION_CREATION *);
  303 (* SESSION_DESTRUCTION *);
  304 (* SESSION_DURATION *);
  305 (* SESSION_NB_TASKS *);
    0|]

let yytransl_block = [|
  268 (* INT *);
  269 (* STRING *);
  270 (* REF *);
  271 (* DATEFMT *);
  272 (* TIMEFMT *);
  273 (* TIMEMSFMT *);
  274 (* DURATIONFMT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\004\000\
\004\000\004\000\004\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\003\000\
\003\000\003\000\011\000\011\000\011\000\011\000\011\000\011\000\
\007\000\009\000\006\000\006\000\005\000\005\000\005\000\005\000\
\005\000\005\000\000\000"

let yylen = "\002\000\
\002\000\001\000\004\000\003\000\003\000\003\000\001\000\003\000\
\003\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000\002\000\
\002\000\001\000\003\000\003\000\003\000\001\000\003\000\001\000\
\001\000\001\000\003\000\003\000\002\000\002\000\003\000\003\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\
\000\000\026\000\000\000\000\000\000\000\000\000\000\000\000\000\
\030\000\000\000\000\000\000\000\000\000\000\000\000\000\051\000\
\000\000\007\000\032\000\033\000\034\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\045\000\046\000\047\000\
\048\000\049\000\050\000\012\000\000\000\000\000\000\000\023\000\
\024\000\025\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\037\000\038\000\000\000\000\000\001\000\000\000\
\000\000\004\000\014\000\015\000\020\000\022\000\013\000\016\000\
\017\000\041\000\043\000\044\000\000\000\000\000\000\000\019\000\
\042\000\027\000\028\000\029\000\021\000\018\000\031\000\035\000\
\036\000\039\000\040\000\000\000\005\000\008\000\009\000\003\000"

let yydgoto = "\002\000\
\032\000\033\000\034\000\052\000\053\000\085\000\086\000\035\000\
\090\000\036\000\037\000"

let yysindex = "\001\000\
\252\254\000\000\252\254\016\255\040\255\052\255\055\255\056\255\
\057\255\061\255\074\255\000\000\059\255\066\255\252\254\252\254\
\252\254\000\000\074\255\074\255\074\255\068\255\074\255\074\255\
\000\000\078\255\082\255\074\255\074\255\074\255\074\255\000\000\
\054\255\000\000\000\000\000\000\000\000\075\255\070\255\058\255\
\080\255\083\255\084\255\085\255\086\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\076\255\252\254\087\255\000\000\
\000\000\000\000\077\255\089\255\090\255\091\255\077\255\077\255\
\092\255\093\255\000\000\000\000\077\255\095\255\000\000\252\254\
\252\254\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\088\255\246\254\079\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\097\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\255\046\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\065\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\253\255\000\000\002\000\029\000\023\000\025\000\000\000\
\196\255\000\000\000\000"

let yytablesize = 110
let yytable = "\038\000\
\011\000\001\000\094\000\095\000\003\000\083\000\084\000\011\000\
\098\000\011\000\011\000\056\000\057\000\058\000\004\000\005\000\
\006\000\007\000\008\000\009\000\010\000\039\000\011\000\012\000\
\013\000\014\000\015\000\016\000\017\000\067\000\068\000\018\000\
\019\000\020\000\021\000\022\000\023\000\024\000\025\000\026\000\
\027\000\028\000\029\000\030\000\031\000\040\000\010\000\059\000\
\060\000\061\000\087\000\063\000\064\000\010\000\071\000\010\000\
\010\000\041\000\069\000\070\000\042\000\043\000\044\000\072\000\
\073\000\006\000\045\000\054\000\100\000\101\000\076\000\055\000\
\006\000\062\000\006\000\046\000\047\000\048\000\049\000\050\000\
\051\000\075\000\074\000\065\000\072\000\073\000\104\000\066\000\
\072\000\073\000\082\000\083\000\084\000\077\000\089\000\078\000\
\079\000\080\000\081\000\088\000\091\000\092\000\082\000\093\000\
\096\000\097\000\099\000\073\000\103\000\102\000"

let yycheck = "\003\000\
\001\001\001\000\063\000\064\000\009\001\016\001\017\001\008\001\
\069\000\010\001\011\001\015\000\016\000\017\000\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\006\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\028\000\029\000\036\001\
\037\001\038\001\039\001\040\001\041\001\042\001\043\001\044\001\
\045\001\046\001\047\001\048\001\049\001\006\001\001\001\019\000\
\020\000\021\000\054\000\023\000\024\000\008\001\001\001\010\001\
\011\001\006\001\030\000\031\000\006\001\006\001\006\001\010\001\
\011\001\001\001\006\001\009\001\072\000\073\000\013\001\006\001\
\008\001\006\001\010\001\002\001\003\001\004\001\005\001\006\001\
\007\001\012\001\008\001\006\001\010\001\011\001\008\001\006\001\
\010\001\011\001\015\001\016\001\017\001\014\001\018\001\013\001\
\013\001\013\001\013\001\013\001\012\001\012\001\015\001\013\001\
\013\001\013\001\012\001\011\001\086\000\085\000"

let yynames_const = "\
  END\000\
  LE\000\
  LEQ\000\
  GE\000\
  GEQ\000\
  EQ\000\
  NE\000\
  RPAR\000\
  LPAR\000\
  OR\000\
  AND\000\
  THREAD_ID\000\
  THREAD_NAME\000\
  TASK_REF\000\
  TASK_NAME\000\
  HOST\000\
  LEVEL\000\
  KEY\000\
  TIME\000\
  DATE\000\
  ALL\000\
  NOT\000\
  MSG\000\
  TASK_ROOT\000\
  TASK_PARENTS\000\
  TASK_CHILDREN\000\
  TASK_EXCEPTIONS\000\
  TASK_ERRORS\000\
  TASK_IN_DB\000\
  TASK_DURATION\000\
  TASK_LINES\000\
  TASK_NB_CHILDREN\000\
  TASK_UUID\000\
  GAP_DURATION\000\
  TASK_GAP_DURATION\000\
  TASK_NOT_COMPLETED\000\
  SESSION_TRACKID\000\
  SESSION_UNAME\000\
  SESSION_CREATION\000\
  SESSION_DESTRUCTION\000\
  SESSION_DURATION\000\
  SESSION_NB_TASKS\000\
  "

let yynames_block = "\
  INT\000\
  STRING\000\
  REF\000\
  DATEFMT\000\
  TIMEFMT\000\
  TIMEMSFMT\000\
  DURATIONFMT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 49 "parser.mly"
           ( _1 )
# 297 "parser.ml"
               : Filter.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                      ( Filter.All        )
# 303 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 54 "parser.mly"
                      ( Filter.Not _3     )
# 310 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 55 "parser.mly"
                      ( _2                )
# 317 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 56 "parser.mly"
                      ( Filter.And[_1;_3] )
# 325 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 57 "parser.mly"
                      ( Filter.Or [_1;_3] )
# 333 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'item) in
    Obj.repr(
# 58 "parser.mly"
                      ( Filter.Item _1    )
# 340 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compare) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'time) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'date) in
    Obj.repr(
# 62 "parser.mly"
                    ( Filter.Full_date (_1, Date.make2 ~date:_3 ~time:_2) )
# 349 "parser.ml"
               : 'compare_date))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compare) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'date) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'time) in
    Obj.repr(
# 63 "parser.mly"
                    ( Filter.Full_date (_1, Date.make2 ~date:_2 ~time:_3) )
# 358 "parser.ml"
               : 'compare_date))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'compare) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'date) in
    Obj.repr(
# 64 "parser.mly"
                    ( Filter.Date (_1,_2)                                 )
# 366 "parser.ml"
               : 'compare_date))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'compare) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'time) in
    Obj.repr(
# 65 "parser.mly"
                    ( Filter.Time (_1,_2)                                 )
# 374 "parser.ml"
               : 'compare_date))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'compare_date) in
    Obj.repr(
# 69 "parser.mly"
                                ( Filter.Log_date _2                   )
# 381 "parser.ml"
               : 'log))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 70 "parser.mly"
                                ( Filter.Log_host _3                        )
# 388 "parser.ml"
               : 'log))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 71 "parser.mly"
                                ( Filter.Log_thread_id _3                   )
# 395 "parser.ml"
               : 'log))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 72 "parser.mly"
                                ( Filter.Log_thread_name _3                 )
# 402 "parser.ml"
               : 'log))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 73 "parser.mly"
                                ( Filter.Log_level (Log.level_of_string _3) )
# 409 "parser.ml"
               : 'log))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 74 "parser.mly"
                                ( Filter.Log_key _3                         )
# 416 "parser.ml"
               : 'log))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'compare) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration) in
    Obj.repr(
# 75 "parser.mly"
                                ( Filter.Log_gap_duration (_2, _3)          )
# 424 "parser.ml"
               : 'log))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 76 "parser.mly"
                                ( Filter.Log_msg _3                         )
# 431 "parser.ml"
               : 'log))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "parser.mly"
                                     ( Filter.Task_ref _3                 )
# 438 "parser.ml"
               : 'task))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "parser.mly"
                                     ( Filter.Task_uuid _3                )
# 445 "parser.ml"
               : 'task))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 82 "parser.mly"
                                     ( Filter.Task_name _3                )
# 452 "parser.ml"
               : 'task))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                                     ( Filter.Task_root _2                )
# 459 "parser.ml"
               : 'task))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                                     ( Filter.Task_parents _2             )
# 466 "parser.ml"
               : 'task))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
                                     ( Filter.Task_children _2            )
# 473 "parser.ml"
               : 'task))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                                     ( Filter.Task_in_database            )
# 479 "parser.ml"
               : 'task))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'compare) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration) in
    Obj.repr(
# 87 "parser.mly"
                                     ( Filter.Task_duration (_2, _3)      )
# 487 "parser.ml"
               : 'task))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'compare) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 88 "parser.mly"
                                     ( Filter.Task_lines (_2, _3)         )
# 495 "parser.ml"
               : 'task))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'compare) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 89 "parser.mly"
                                     ( Filter.Task_nb_children (_2,_3)    )
# 503 "parser.ml"
               : 'task))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
                                     ( Filter.Task_not_completed          )
# 509 "parser.ml"
               : 'task))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'compare) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration) in
    Obj.repr(
# 91 "parser.mly"
                                     ( Filter.Task_gap_duration (_2, _3)  )
# 517 "parser.ml"
               : 'task))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'log) in
    Obj.repr(
# 95 "parser.mly"
          ( Filter.Log _1     )
# 524 "parser.ml"
               : 'item))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'task) in
    Obj.repr(
# 96 "parser.mly"
          ( Filter.Task _1    )
# 531 "parser.ml"
               : 'item))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'session) in
    Obj.repr(
# 97 "parser.mly"
          ( Filter.Session _1 )
# 538 "parser.ml"
               : 'item))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 100 "parser.mly"
                                    ( Filter.Session_trackid _3         )
# 545 "parser.ml"
               : 'session))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 101 "parser.mly"
                                    ( Filter.Session_uname _3           )
# 552 "parser.ml"
               : 'session))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'compare_date) in
    Obj.repr(
# 102 "parser.mly"
                                    ( Filter.Session_creation _2        )
# 559 "parser.ml"
               : 'session))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'compare_date) in
    Obj.repr(
# 103 "parser.mly"
                                    ( Filter.Session_destruction _2     )
# 566 "parser.ml"
               : 'session))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'compare) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'duration) in
    Obj.repr(
# 104 "parser.mly"
                                    ( Filter.Session_duration  (_2, _3) )
# 574 "parser.ml"
               : 'session))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'compare) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 105 "parser.mly"
                                    ( Filter.Session_nb_tasks (_2, _3)  )
# 582 "parser.ml"
               : 'session))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 108 "parser.mly"
          ( Scanf.sscanf _1 "%04d%02d%02d" (fun y m d -> Date.make_date ~day:d ~month:m ~year:y) )
# 589 "parser.ml"
               : 'date))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 112 "parser.mly"
              ( Date.Duration.of_string _1 )
# 596 "parser.ml"
               : 'duration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 116 "parser.mly"
            ( Scanf.sscanf _1 "%02d:%02d:%02d" (fun hours minutes seconds -> Date.make_time ~hours ~minutes ~seconds ~ms:0)     )
# 603 "parser.ml"
               : 'time))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 117 "parser.mly"
            ( Scanf.sscanf _1 "%02d:%02d:%02d.%d" (fun hours minutes seconds ms -> Date.make_time ~hours ~minutes ~seconds ~ms) )
# 610 "parser.ml"
               : 'time))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "parser.mly"
      ( Filter.LE  )
# 616 "parser.ml"
               : 'compare))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "parser.mly"
      ( Filter.LEQ )
# 622 "parser.ml"
               : 'compare))
; (fun __caml_parser_env ->
    Obj.repr(
# 123 "parser.mly"
      ( Filter.GE  )
# 628 "parser.ml"
               : 'compare))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "parser.mly"
      ( Filter.GEQ )
# 634 "parser.ml"
               : 'compare))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "parser.mly"
      ( Filter.EQ  )
# 640 "parser.ml"
               : 'compare))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "parser.mly"
      ( Filter.NE  )
# 646 "parser.ml"
               : 'compare))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Filter.t)
