%%{
  machine message;
  
  action mark_pos { _pos := !p }
  action is_exception { 
    error := Log.Got_exception; 
    _pos := !p;
  }
  action is_error { 
    error := Log.Got_error; 
  }
  action message { message := get_current_string () }

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

  is_exception = 'Raised at ' >is_exception . any*;
  is_error = any* >mark_pos :> 'Got exception' >is_error . any*; 
  is_internal = any* >mark_pos :> 'INTERNAL' >is_error . any*; 
  
  trackid = xdigit+ >mark_pos %trackid;
#  parent_trackid = xdigit+ >mark_pos %parent_trackid;
  pool = ('true' @pool_true | 'false');
  uname = 'uname=' . (((any - ' ')+ >mark_pos) %uname)? . space;
  local_su = 'is_local_superuser=' . ('true' @local_su_true | 'false');
  auth = 'auth_user_sid=' . (((any - ' ')+ >mark_pos) %auth)? . space;
  session_created_part := (local_su . space . auth) @session_created  . (any*) ;
  uuid = (xdigit | '-')+;
  opaqueref = 'OpaqueRef:' . uuid;

  session_created = 'Session.create trackid=' . trackid . space . 'pool=' pool . space . 
    uname @{fgoto session_created_part; };
  session_destroyed = ('Session.destroy trackid=' . trackid) %session_destroyed; 
  session_gc = ('Session.destroy _ref=' . opaqueref . space . 'uuid=' . uuid . space .
      'trackid=' . trackid) %session_destroyed . (any*);

  message = (
    is_exception | is_error | is_internal |
        session_destroyed | session_gc | session_created |
        (any* >mark_pos) 
  ) %/message;

}%%
