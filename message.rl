%%{
  machine message;
  
  action mark_pos { _pos := !p }
  action mark_mesg_pos { message := String.sub data !p (!pe - !p) }
  action is_exception { error := Log.Got_exception; }
  action is_error { error := Log.Got_error; }

  action trackid { trackid := get_current_string () }
  action pool_true { pool := true }
  action uname { uname := get_current_string () }
  action local_su_true { local_su := true }
  action auth { auth := get_current_string () }
#  action parent_trackid { parent_trackid := get_current_string () }
  action session_created {
    let d = match !date with None -> fail "Date not created"
      | Some v -> v in
    session_log := Some (Session (Session.create ~trackid:!trackid ~pool:!pool 
                                    ~uname:!uname ~is_local_superuser:!local_su 
                                    ~auth_user_sid:!auth ~creation:d))
  }
  action session_destroyed {
    let d = match !date with None -> fail "Date not created"
      | Some v -> v in
    session_log := Some (Destruction (!trackid, d))
  }

  action task_destroyed {
    mesg_task := TaskDestruction
  }
  action task_created {
    mesg_task := TaskCreation(!mesg_task_name, !mesg_task_ref, !mesg_task_uuid, !mesg_task_trackid,
                              !mesg_parent_task_ref, false)
  }
  action task_forwarded {
    mesg_task := TaskCreation(!mesg_task_name, !mesg_task_ref, !mesg_task_uuid, !mesg_task_trackid,
                              !mesg_parent_task_ref, true)
  }
  action task_async {
    mesg_task := TaskAsync(!mesg_task_trackid)
  }

  action mesg_task_name { mesg_task_name := Util.uniquify cache_task_names (String.sub data !_pos (!p - !_pos - 1));}
  action mesg_task_ref { mesg_task_ref := get_current_string (); }
  action mesg_task_uuid { mesg_task_uuid := Some (get_current_string ());}
  action mesg_task_trackid { mesg_task_trackid := Some (get_current_string ());}
  action mesg_parent_task_ref { mesg_parent_task_ref := Some (get_current_string ());}
  action async { mesg_task_async := true }

  is_exception = 'Raised at' >is_exception >{fbreak;};
  is_error = any* :> 'Got exception' >is_error >{fbreak;} ; 
  is_internal = any* :> 'INTERNAL' >is_error . (any*); 

  task_destroyed = 'forwarded task destroyed' @task_destroyed;
  
  mesg_task_ref = (('D' | 'R') ':' xdigit+) >mark_pos %mesg_task_ref;
  mesg_task_name_ref = (any+ >mark_pos space %mesg_task_name) 
                   :> mesg_task_ref;
  mesg_parent_task_ref = (('D' | 'R') ':' xdigit+) >mark_pos %mesg_parent_task_ref;
  mesg_task_uuid = '(uuid:' . (xdigit | '-')+ >mark_pos %mesg_task_uuid . ')'; 
  mesg_task_trackid = '(trackid=' . xdigit+ >mark_pos %mesg_task_trackid . ')';

  task_created = ('task ' . mesg_task_name_ref . (space . mesg_task_uuid)? 
    . space. 'created' . (space . mesg_task_trackid)? .space . 'by task' . space . 
    (mesg_parent_task_ref | any*)) %task_created ;

  task_forwarded = ('task ' . mesg_task_name_ref . (space . mesg_task_uuid)? 
    . space. 'forwarded' . (space . mesg_task_trackid)?) %task_forwarded ;

  task_async = ('spawning a new thread to handle the current task' . space . mesg_task_trackid) %task_async;
  trackid = xdigit+ >mark_pos %trackid;
#  parent_trackid = xdigit+ >mark_pos %parent_trackid;
  pool = ('true' @pool_true | 'false');
  uname = 'uname=' . (((any - ' ')+ >mark_pos) %uname)? . space;
  local_su = 'is_local_superuser=' . ('true' @local_su_true | 'false');
  auth = 'auth_user_sid=' . (((any - ' ')+ >mark_pos) %auth)? . space;
  session_created_part := (local_su . space . auth) @session_created @{fbreak;}  ;
  uuid = (xdigit | '-')+;
  opaqueref = 'OpaqueRef:' . uuid;

  session_created = 'Session.create trackid=' . trackid . space . 'pool=' pool . space . 
    uname @{fgoto session_created_part; };
  session_destroyed = ('Session.destroy trackid=' . trackid) %session_destroyed; 
  session_gc = ('Session.destroy _ref=' . opaqueref . space . 'uuid=' . uuid . space .
      'trackid=' . trackid) %session_destroyed %{fbreak;};
  
  error_msg = is_exception | is_error | is_internal;
  session_msg = session_destroyed | session_gc | session_created;
  task_msg = task_destroyed | task_created | task_forwarded | task_async;
  message = (
    error_msg | session_msg | task_msg |
        (any*)
  ) >mark_mesg_pos;

}%%
