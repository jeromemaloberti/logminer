{
(* Copyright (c) Citrix Systems 2008-2009. All rights reserved.  *)
(* Author: Thomas Gazagnaire <thomas.gazagnaire@citrix.com>      *)

  let debug (fmt: ('a, unit, string, unit) format4) : 'a = 
    Printf.kprintf (fun s -> () (*Printf.printf "[%s]" s*)) fmt
}
let num = ['0'-'9']
let date = ("19" | "20") num num ['0'-'1'] num ['0'-'3'] num
let duration = (['0'-'9']+"d")? (['0'-'9']+"h")? (['0'-'9']+"m")? (['0'-'9' '.']+"s")?
let time = num num ":" num num ":" num num
let timems = time "." num+

rule main = parse
  | "all" { debug "all"; Parser.ALL }

  | "date"  { debug "date"; Parser.DATE }

  | "thread-id"    { debug "thread_id"; Parser.THREAD_ID     }
  | "thread-name"  { debug "thread_name"; Parser.THREAD_NAME }
  | "host"         { debug "host"; Parser.HOST               }
  | "level"        { debug "level"; Parser.LEVEL             }
  | "key"          { debug "key"; Parser.KEY                 }
  | "message"      | "msg" { debug "message"; Parser.MSG           }
  | "gap-duration" | "gap" { debug "gap-duration"; Parser.GAP_DURATION }

  | "task-ref"         | "tref"        { debug "task-ref"; Parser.TASK_REF                     }
  | "task-uuid"        | "tuuid"       { debug "task-uuid"; Parser.TASK_UUID                   }
  | "task-name"        | "tname"       { debug "task-name"; Parser.TASK_NAME                   }
  | "task-root"        | "troot"       { debug "task-root"; Parser.TASK_ROOT                   }
  | "task-parents"     | "tparents"    { debug "task-parents"; Parser.TASK_PARENTS             }
  | "task-children"    | "tchildren"   { debug "task-children"; Parser.TASK_CHILDREN           }
  | "task-in-database" | "tdb"         { debug "task-in-database"; Parser.TASK_IN_DB           }
  | "task-duration"    | "tduration"   { debug "task-duration"; Parser.TASK_DURATION           }
  | "task-lines"       | "tlines"      { debug "task-lines"; Parser.TASK_LINES                 }
  | "task-nb-children" | "tnbchildren" { debug "task-nb-children"; Parser.TASK_NB_CHILDREN     }
  | "task-not-completed" | "tnc"       { debug "task-not-completed"; Parser.TASK_NOT_COMPLETED }
  | "task-gap-duration"| "tgap"        { debug "time-gap"; Parser.TASK_GAP_DURATION            }

  | "session-trackid" | "strackid"        { debug "session-trackid"; Parser.SESSION_TRACKID         }
  | "session-uname"   | "suname"          { debug "session-uname"; Parser.SESSION_UNAME             }
  | "session-creation"   | "screation"    { debug "session-creation"; Parser.SESSION_CREATION       }
  | "session-destruction"| "sdestruction" { debug "session-destruction"; Parser.SESSION_DESTRUCTION }
  | "session-duration" | "sduration"      { debug "session-duration"; Parser.SESSION_DURATION       }
  | "session-nb-tasks" | "snbtasks"       { debug "session-nb-tasks"; Parser.SESSION_NB_TASKS       }

  | '<'  { debug "<"; Parser.LE    }
  | "<=" { debug "<="; Parser.LEQ  }
  | '>'  { debug ">"; Parser.GE    }
  | ">=" { debug ">="; Parser.GEQ  }
  | '='  { debug "="; Parser.EQ    }
  | "!=" { debug "!="; Parser.NE   }

  | "not" | "NOT" | "!" { debug "NOT"; Parser.NOT }

  | '('  { debug "("; Parser.LPAR  }
  | ')'  { debug ")"; Parser.RPAR  }

  | '|' | "||" | "OR"  | "or"   { debug "||"; Parser.OR    }
  | '&' | "&&" | "AND" | "and"  { debug "&&"; Parser.AND   }

  | ['D' 'R'] ":" ['0'-'9' 'a'-'f']+       as r { debug "ref: %s" r; Parser.REF r                 }
  | date                                   as d { debug "date: %s" d; Parser.DATEFMT d            }
  | duration                               as d { debug "duration: %s" d; Parser.DURATIONFMT d    }
  | time                                   as t { debug "time: %s" t; Parser.TIMEFMT t            }
  | timems                                 as t { debug "timems: %s" t; Parser.TIMEMSFMT t        }
  | ['0'-'9']+                             as n { debug "int: %s" n; Parser.INT (int_of_string n) }
  | ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '.']+ as s { debug "string: %s" s; Parser.STRING s           }
  | '\"' ([^'\"']* as s) '\"'                   { debug "qstring: %s" s; Parser.STRING s          }
  | '\'' ([^'\'']* as s) '\''                   { debug "dqstring: %s" s; Parser.STRING s         }

  | [' ']    { debug "space"; main lexbuf }
  | [';']    { debug "end"; Parser.END    }

