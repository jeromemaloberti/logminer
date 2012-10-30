%{
(* Copyright (c) Citrix Systems 2008-2009. All rights reserved. *)
(* Author: Thomas Gazagnaire <thomas.gazagnaire@citrix.com>     *)

let parse_error s = (* Called by the parser function on error *)
    Printf.eprintf "query: %s on characters %i-%i\n" s (symbol_start ()) (symbol_end ());
    flush stdout;
    exit 4
%}

%token END
%token LE LEQ GE GEQ EQ NE
%token RPAR LPAR OR AND
%token <int> INT
%token <string> STRING REF DATEFMT TIMEFMT TIMEMSFMT DURATIONFMT
%token THREAD_ID THREAD_NAME TASK_REF TASK_NAME HOST LEVEL KEY
%token TIME DATE ALL NOT MSG
%token TASK_ROOT TASK_PARENTS TASK_CHILDREN TASK_EXCEPTIONS TASK_ERRORS TASK_IN_DB TASK_DURATION
%token TASK_LINES TASK_NB_CHILDREN TASK_UUID GAP_DURATION TASK_GAP_DURATION
%token TASK_NOT_COMPLETED
%token SESSION_TRACKID SESSION_UNAME SESSION_CREATION SESSION_DESTRUCTION SESSION_DURATION
%token SESSION_NB_TASKS

%left OR
%left AND
%left NOT
%left LE LEQ GE GEQ EQ NE
%left LPAR RPAR
%left TASK_ROOT TASK_PARENTS TASK_CHILDREN 

%start main
%type <Filter.t> main

%%

main:
| expr END { $1 }
;

expr:
| ALL                 { Filter.All        }
| NOT LPAR expr RPAR  { Filter.Not $3     }
| LPAR expr RPAR      { $2                }
| expr AND expr       { Filter.And[$1;$3] }
| expr OR expr        { Filter.Or [$1;$3] }
| item                { Filter.Item $1    }
;

compare_date:
| compare time date { Filter.Full_date ($1, Date.make2 ~date:$3 ~time:$2) }
| compare date time { Filter.Full_date ($1, Date.make2 ~date:$2 ~time:$3) }
| compare date      { Filter.Date ($1,$2)                                 }
| compare time      { Filter.Time ($1,$2)                                 }
;

log:
| DATE compare_date             { Filter.Log_date $2                   }
| HOST EQ STRING                { Filter.Log_host $3                        }
| THREAD_ID EQ INT              { Filter.Log_thread_id $3                   }
| THREAD_NAME EQ STRING         { Filter.Log_thread_name $3                 }
| LEVEL EQ STRING               { Filter.Log_level (Log.level_of_string $3) }
| KEY EQ STRING                 { Filter.Log_key $3                         }
| GAP_DURATION compare duration { Filter.Log_gap_duration ($2, $3)          }
| MSG EQ STRING                 { Filter.Log_msg $3                         }
;

task:
| TASK_REF EQ REF                    { Filter.Task_ref $3                 }
| TASK_UUID EQ STRING                { Filter.Task_uuid $3                }
| TASK_NAME EQ STRING                { Filter.Task_name $3                }
| TASK_ROOT expr                     { Filter.Task_root $2                }
| TASK_PARENTS expr                  { Filter.Task_parents $2             }
| TASK_CHILDREN expr                 { Filter.Task_children $2            }
| TASK_IN_DB                         { Filter.Task_in_database            }
| TASK_DURATION compare duration     { Filter.Task_duration ($2, $3)      }
| TASK_LINES compare INT             { Filter.Task_lines ($2, $3)         }
| TASK_NB_CHILDREN compare INT       { Filter.Task_nb_children ($2,$3)    }
| TASK_NOT_COMPLETED                 { Filter.Task_not_completed          }
| TASK_GAP_DURATION compare duration { Filter.Task_gap_duration ($2, $3)  }
;

item:
| log     { Filter.Log $1     }
| task    { Filter.Task $1    }
| session { Filter.Session $1 }
;
session:
| SESSION_TRACKID EQ STRING         { Filter.Session_trackid $3         }
| SESSION_UNAME EQ STRING           { Filter.Session_uname $3           }
| SESSION_CREATION compare_date     { Filter.Session_creation $2        }
| SESSION_DESTRUCTION compare_date  { Filter.Session_destruction $2     }
| SESSION_DURATION compare duration { Filter.Session_duration  ($2, $3) }
| SESSION_NB_TASKS compare INT      { Filter.Session_nb_tasks ($2, $3)  }

date:
| DATEFMT { Scanf.sscanf $1 "%04d%02d%02d" (fun y m d -> Date.make_date ~day:d ~month:m ~year:y) }
;

duration:
| DURATIONFMT { Date.Duration.of_string $1 }
;
	
time:
| TIMEFMT   { Scanf.sscanf $1 "%02d:%02d:%02d" (fun hours minutes seconds -> Date.make_time ~hours ~minutes ~seconds ~ms:0)     }
| TIMEMSFMT { Scanf.sscanf $1 "%02d:%02d:%02d.%d" (fun hours minutes seconds ms -> Date.make_time ~hours ~minutes ~seconds ~ms) }
;

compare:
| LE  { Filter.LE  }
| LEQ { Filter.LEQ }
| GE  { Filter.GE  }
| GEQ { Filter.GEQ }
| EQ  { Filter.EQ  }
| NE  { Filter.NE  }
;
