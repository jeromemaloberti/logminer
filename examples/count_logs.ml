Ocaml.packs := ["logminer"]
--


let print_top_logs f =
  let db = Logminer.load_file f in
  let a = Logminer.count_logs db in
  Array.iter (fun (l,n) -> Printf.printf "%d %s\n" !n l) a

let _ =
  let file = 
    match Sys.argv with
      [| _; s |] -> s
    | _ -> prerr_endline "You need to specify the file."; exit 1
  in
  print_top_logs file
  
