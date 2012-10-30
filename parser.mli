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
