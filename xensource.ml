
# 1 "xensource.rl"
let quiet = ref false

let info (fmt: ('a, unit, string, unit) format4) : 'a =
	Printf.kprintf (fun s -> if not !quiet then print_string s; flush stdout) fmt

let debug (fmt: ('a, unit, string, unit) format4) : 'a =
	Printf.kprintf (fun s -> print_endline s; flush stdout) fmt

let fail fmt = Printf.ksprintf failwith fmt

let cache_task_names = Hashtbl.create 128
let cache_thread_infos = Hashtbl.create 16
let cache_hosts = Hashtbl.create 16
let cache_keys = Hashtbl.create 32

exception Goto_out

let line_num = ref 0


# 24 "xensource.ml"
let _xensource_trans_keys : int array = [|
	0; 0; 65; 83; 112; 117; 114; 114; 9; 32; 9; 57; 48; 57; 9; 32; 
	48; 57; 48; 57; 58; 58; 48; 57; 48; 57; 58; 58; 48; 57; 48; 57; 
	9; 32; 9; 32; 9; 32; 9; 108; 9; 58; 9; 32; 91; 91; 32; 101; 
	105; 119; 110; 110; 102; 102; 111; 111; 124; 124; 124; 124; 124; 124; 48; 57; 
	9; 124; 124; 124; 124; 124; 124; 124; 68; 124; 124; 124; 93; 93; 93; 93; 
	9; 32; 97; 97; 114; 114; 110; 110; 101; 101; 98; 98; 117; 117; 103; 103; 
	114; 114; 114; 114; 111; 111; 114; 114; 9; 58; 9; 58; 9; 47; 9; 47; 
	9; 58; 9; 97; 9; 115; 9; 116; 9; 58; 109; 109; 101; 101; 115; 115; 
	115; 115; 97; 97; 103; 103; 101; 101; 32; 32; 114; 114; 101; 101; 112; 112; 
	101; 101; 97; 97; 116; 116; 101; 101; 100; 100; 48; 57; 103; 103; 101; 101; 
	99; 99; 101; 101; 98; 98; 97; 117; 110; 110; 108; 110; 97; 97; 114; 121; 
	111; 111; 118; 118; 99; 99; 116; 116; 101; 101; 112; 112; 0; 0; 0; 0; 
	0; 0; 0
|]

let _xensource_key_spans : int array = [|
	0; 19; 6; 1; 24; 49; 10; 24; 
	10; 10; 1; 10; 10; 1; 10; 10; 
	24; 24; 24; 100; 50; 24; 1; 70; 
	15; 1; 1; 1; 1; 1; 1; 10; 
	116; 1; 1; 1; 57; 1; 1; 1; 
	24; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 50; 50; 39; 39; 
	50; 89; 107; 108; 50; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 10; 1; 1; 
	1; 1; 1; 21; 1; 3; 1; 8; 
	1; 1; 1; 1; 1; 1; 0; 0; 
	0
|]

let _xensource_index_offsets : int array = [|
	0; 0; 20; 27; 29; 54; 104; 115; 
	140; 151; 162; 164; 175; 186; 188; 199; 
	210; 235; 260; 285; 386; 437; 462; 464; 
	535; 551; 553; 555; 557; 559; 561; 563; 
	574; 691; 693; 695; 697; 755; 757; 759; 
	761; 786; 788; 790; 792; 794; 796; 798; 
	800; 802; 804; 806; 808; 859; 910; 950; 
	990; 1041; 1131; 1239; 1348; 1399; 1401; 1403; 
	1405; 1407; 1409; 1411; 1413; 1415; 1417; 1419; 
	1421; 1423; 1425; 1427; 1429; 1431; 1442; 1444; 
	1446; 1448; 1450; 1452; 1474; 1476; 1480; 1482; 
	1491; 1493; 1495; 1497; 1499; 1501; 1503; 1504; 
	1505
|]

let _xensource_indicies : int array = [|
	0; 1; 1; 2; 1; 3; 1; 
	1; 1; 4; 1; 1; 5; 6; 7; 
	1; 1; 1; 8; 1; 9; 1; 1; 
	1; 1; 10; 1; 11; 1; 12; 12; 
	12; 12; 12; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 12; 1; 13; 
	13; 13; 13; 13; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 13; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 14; 14; 
	14; 14; 14; 14; 14; 14; 14; 14; 
	1; 15; 15; 15; 15; 15; 15; 15; 
	15; 15; 15; 1; 16; 16; 16; 16; 
	16; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 16; 1; 17; 17; 17; 
	17; 17; 17; 17; 17; 17; 17; 1; 
	18; 18; 18; 18; 18; 18; 18; 18; 
	18; 18; 1; 19; 1; 20; 20; 20; 
	20; 20; 20; 20; 20; 20; 20; 1; 
	21; 21; 21; 21; 21; 21; 21; 21; 
	21; 21; 1; 22; 1; 23; 23; 23; 
	23; 23; 23; 23; 23; 23; 23; 1; 
	24; 24; 24; 24; 24; 24; 24; 24; 
	24; 24; 1; 25; 25; 25; 25; 25; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 25; 1; 1; 1; 1; 1; 
	1; 26; 26; 26; 26; 26; 26; 26; 
	26; 26; 26; 26; 26; 26; 26; 26; 
	26; 26; 26; 1; 26; 27; 27; 27; 
	27; 27; 26; 26; 26; 26; 26; 26; 
	26; 26; 26; 26; 26; 26; 26; 26; 
	26; 26; 26; 26; 27; 26; 1; 1; 
	1; 1; 1; 28; 28; 28; 28; 28; 
	28; 28; 28; 28; 28; 28; 28; 28; 
	28; 28; 28; 28; 28; 1; 28; 28; 
	28; 28; 28; 28; 28; 28; 28; 28; 
	28; 28; 28; 28; 29; 28; 28; 28; 
	28; 28; 28; 28; 28; 28; 28; 1; 
	28; 28; 28; 28; 28; 28; 28; 28; 
	28; 28; 28; 28; 28; 28; 28; 28; 
	28; 28; 28; 28; 28; 28; 28; 28; 
	28; 28; 28; 28; 28; 28; 28; 28; 
	28; 28; 28; 28; 28; 28; 28; 28; 
	28; 28; 28; 28; 28; 28; 28; 28; 
	28; 30; 28; 1; 1; 1; 1; 1; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 1; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 32; 31; 33; 33; 
	33; 33; 33; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 33; 1; 34; 
	1; 35; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 36; 37; 1; 
	38; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 39; 1; 
	40; 1; 41; 1; 42; 1; 43; 1; 
	1; 44; 46; 45; 47; 47; 47; 47; 
	47; 47; 47; 47; 47; 47; 1; 48; 
	48; 48; 48; 48; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 48; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 49; 49; 
	49; 49; 49; 49; 49; 49; 49; 49; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 50; 1; 1; 51; 53; 52; 
	55; 54; 57; 56; 56; 56; 56; 56; 
	56; 56; 56; 56; 56; 56; 56; 56; 
	57; 56; 56; 56; 56; 56; 56; 56; 
	56; 56; 56; 56; 56; 56; 56; 56; 
	56; 56; 56; 56; 56; 56; 56; 56; 
	56; 56; 56; 56; 56; 56; 56; 56; 
	56; 56; 56; 56; 56; 56; 56; 56; 
	56; 56; 58; 56; 58; 59; 1; 60; 
	62; 61; 63; 63; 63; 63; 63; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 63; 1; 64; 1; 65; 1; 66; 
	1; 67; 1; 68; 1; 69; 1; 70; 
	1; 71; 1; 72; 1; 73; 1; 74; 
	1; 1; 1; 1; 1; 1; 75; 75; 
	75; 75; 75; 75; 75; 75; 75; 75; 
	75; 75; 75; 75; 75; 75; 75; 75; 
	1; 75; 75; 75; 75; 75; 75; 75; 
	75; 75; 75; 75; 75; 75; 75; 28; 
	75; 75; 75; 75; 75; 75; 75; 75; 
	75; 75; 76; 75; 1; 1; 1; 1; 
	1; 77; 77; 77; 77; 77; 77; 77; 
	77; 77; 77; 77; 77; 77; 77; 77; 
	77; 77; 77; 1; 77; 77; 77; 77; 
	77; 77; 77; 77; 77; 77; 77; 77; 
	77; 77; 78; 77; 77; 77; 77; 77; 
	77; 77; 77; 77; 77; 76; 77; 33; 
	33; 33; 33; 33; 79; 79; 79; 79; 
	79; 79; 79; 79; 79; 79; 79; 79; 
	79; 79; 79; 79; 79; 79; 33; 79; 
	79; 79; 79; 79; 79; 79; 79; 79; 
	79; 79; 79; 79; 79; 80; 79; 1; 
	1; 1; 1; 1; 79; 79; 79; 79; 
	79; 79; 79; 79; 79; 79; 79; 79; 
	79; 79; 79; 79; 79; 79; 1; 79; 
	79; 79; 79; 79; 79; 79; 79; 79; 
	79; 79; 79; 79; 79; 80; 79; 1; 
	1; 1; 1; 1; 75; 75; 75; 75; 
	75; 75; 75; 75; 75; 75; 75; 75; 
	75; 75; 75; 75; 75; 75; 1; 75; 
	75; 75; 75; 75; 75; 75; 75; 75; 
	75; 75; 75; 75; 75; 28; 75; 75; 
	75; 75; 75; 75; 75; 75; 75; 75; 
	79; 75; 1; 1; 1; 1; 1; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 1; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 32; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 81; 31; 1; 1; 1; 1; 
	1; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 1; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 32; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 82; 31; 
	1; 1; 1; 1; 1; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 1; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 32; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 83; 31; 1; 1; 1; 
	1; 1; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 84; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 31; 31; 
	31; 31; 31; 31; 31; 31; 32; 31; 
	85; 1; 86; 1; 87; 1; 88; 1; 
	89; 1; 90; 1; 91; 1; 92; 1; 
	93; 1; 94; 1; 95; 1; 96; 1; 
	97; 1; 98; 1; 99; 1; 100; 1; 
	101; 101; 101; 101; 101; 101; 101; 101; 
	101; 101; 1; 102; 1; 103; 1; 104; 
	1; 105; 1; 106; 1; 107; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 1; 1; 1; 1; 1; 1; 1; 
	1; 108; 1; 109; 1; 110; 1; 111; 
	1; 112; 1; 113; 1; 1; 1; 1; 
	1; 1; 114; 1; 115; 1; 116; 1; 
	117; 1; 118; 1; 119; 1; 120; 1; 
	121; 122; 1; 0
|]

let _xensource_trans_targs : int array = [|
	2; 0; 79; 81; 83; 86; 88; 90; 
	92; 3; 78; 4; 5; 6; 77; 7; 
	8; 9; 10; 11; 12; 13; 14; 15; 
	16; 17; 18; 19; 20; 52; 57; 20; 
	21; 22; 23; 24; 44; 48; 25; 41; 
	26; 27; 28; 29; 30; 30; 31; 32; 
	33; 32; 35; 34; 34; 35; 36; 38; 
	36; 37; 38; 37; 39; 39; 40; 94; 
	42; 43; 28; 45; 46; 47; 28; 49; 
	50; 51; 28; 53; 54; 53; 52; 55; 
	56; 58; 59; 60; 61; 62; 63; 64; 
	65; 66; 67; 68; 69; 70; 71; 72; 
	73; 74; 75; 76; 96; 7; 4; 80; 
	4; 82; 4; 84; 85; 4; 4; 4; 
	87; 4; 4; 89; 4; 91; 4; 93; 
	4; 95; 95
|]

let _xensource_trans_actions : int array = [|
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 1; 0; 0; 2; 3; 
	0; 2; 4; 0; 2; 5; 0; 2; 
	6; 0; 0; 0; 7; 7; 7; 0; 
	8; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 9; 0; 7; 0; 10; 11; 
	0; 12; 0; 7; 0; 13; 7; 0; 
	0; 14; 15; 0; 7; 0; 16; 0; 
	0; 0; 17; 0; 0; 0; 18; 0; 
	0; 0; 19; 7; 8; 0; 0; 0; 
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 20; 21; 22; 0; 
	23; 0; 24; 0; 0; 25; 26; 27; 
	0; 28; 29; 0; 30; 0; 31; 0; 
	32; 33; 0
|]

let _xensource_eof_actions : int array = [|
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 0; 0; 0; 0; 
	0; 0; 0; 0; 0; 0; 33; 0; 
	0
|]

let xensource_start : int = 1
let xensource_first_final : int = 94
let xensource_error : int = 0

let xensource_en_main : int = 1

type state = { mutable keys : int; mutable trans : int; }
exception Goto_match
exception Goto_again
exception Goto_eof_trans

# 24 "xensource.rl"


let parse_xensource ?log_counter data =
  let cs = ref 0 in
  let p = ref 0 in
  let pe = ref (String.length data) in
  let eof = ref !pe in
  let month = ref 0 in
  let day = ref 0 in
  let hour = ref 0 in
  let min = ref 0 in
  let sec = ref 0 in
  let thread_id = ref 0 in
  let thread_info = ref None in
  let level = ref Log.Debug in
  let host = ref "" in
  let empty_log_line = ref false in 
  let task_name = ref None in
  let task_ref = ref None in
  let task_ref_bool = ref false in
  let key = ref "" in
  let message = ref "" in
  let _int = ref 0 in 
  let _pos = ref 0 in
  let get_current_string () = String.sub data !_pos (!p - !_pos) in
  
  let date = ref None in

# 359 "xensource.ml"
	begin
	cs.contents <- xensource_start;
	end;

# 98 "xensource.rl"

  begin
    try 

# 369 "xensource.ml"
	begin
	let state = { keys = 0; trans = 0; } in
	let rec do_start () =
	if p.contents = pe.contents then
		do_test_eof ()
	else
	if cs.contents = 0 then
		do_out ()
	else
	do_resume ()
and do_resume () =
	begin try
	let keys = cs.contents lsl 1 in
	let inds = _xensource_index_offsets.(cs.contents) in

	let slen = _xensource_key_spans.(cs.contents) in
	state.trans <- _xensource_indicies.(inds + (
		if slen > 0 && _xensource_trans_keys.(keys) <= Char.code data.[p.contents] &&
		Char.code data.[p.contents] <= _xensource_trans_keys.(keys+1) then
		Char.code data.[p.contents] - _xensource_trans_keys.(keys) else slen));

	with Goto_match -> () end;
	do_eof_trans ()
and do_eof_trans () =
	cs.contents <- _xensource_trans_targs.(state.trans);

	begin try if _xensource_trans_actions.(state.trans) = 0 then
		raise Goto_again;

	match _xensource_trans_actions.(state.trans) with
	| 25 ->
# 19 "date_time.rl"
		begin  month := 0  end;
	()
	| 24 ->
# 20 "date_time.rl"
		begin  month := 1  end;
	()
	| 28 ->
# 21 "date_time.rl"
		begin  month := 2  end;
	()
	| 1 ->
# 22 "date_time.rl"
		begin  month := 3  end;
	()
	| 29 ->
# 23 "date_time.rl"
		begin  month := 4  end;
	()
	| 27 ->
# 24 "date_time.rl"
		begin  month := 5  end;
	()
	| 26 ->
# 25 "date_time.rl"
		begin  month := 6  end;
	()
	| 22 ->
# 26 "date_time.rl"
		begin  month := 7  end;
	()
	| 32 ->
# 27 "date_time.rl"
		begin  month := 8  end;
	()
	| 31 ->
# 28 "date_time.rl"
		begin  month := 9  end;
	()
	| 30 ->
# 29 "date_time.rl"
		begin  month := 10  end;
	()
	| 23 ->
# 30 "date_time.rl"
		begin  month := 11  end;
	()
	| 8 ->
# 52 "xensource.rl"
		begin   end;
	()
	| 7 ->
# 53 "xensource.rl"
		begin  _pos := !p  end;
	()
	| 13 ->
# 59 "xensource.rl"
		begin  thread_info := Some (Util.uniquify cache_thread_infos (get_current_string ()))  end;
	()
	| 9 ->
# 60 "xensource.rl"
		begin  level := Log.Info  end;
	()
	| 17 ->
# 61 "xensource.rl"
		begin  level := Log.Warn  end;
	()
	| 18 ->
# 62 "xensource.rl"
		begin  level := Log.Debug  end;
	()
	| 19 ->
# 63 "xensource.rl"
		begin  level := Log.Error  end;
	()
	| 10 ->
# 64 "xensource.rl"
		begin  host := Util.uniquify cache_hosts (get_current_string ())  end;
	()
	| 16 ->
# 65 "xensource.rl"
		begin  key := Util.uniquify cache_keys (get_current_string ())  end;
	()
	| 20 ->
# 66 "xensource.rl"
		begin  empty_log_line := true; begin p.contents <- p.contents + 1; if true then raise Goto_out end  end;
	()
	| 15 ->
# 67 "xensource.rl"
		begin  
    if !task_ref_bool then begin
      task_ref := Some (String.sub data (!p - 14) 14);
      task_name := Some (Util.uniquify cache_task_names (String.sub data !_pos (!p - !_pos - 15)));
    end else begin
      task_name := Some (Util.uniquify cache_task_names (get_current_string ()))
    end
   end;
	()
	| 14 ->
# 75 "xensource.rl"
		begin  task_ref_bool := true;  end;
	()
	| 33 ->
# 76 "xensource.rl"
		begin  message := String.sub data !p (!pe - !p); begin p.contents <- p.contents + 1; if true then raise Goto_out end  end;
	()
	| 2 ->
# 35 "date_time.rl"
		begin  _int := 0  end;
# 36 "date_time.rl"
		begin  _int := !_int * 10 + (Char.code data.[p.contents] - Char.code '0') end;
	()
	| 21 ->
# 36 "date_time.rl"
		begin  _int := !_int * 10 + (Char.code data.[p.contents] - Char.code '0') end;
# 31 "date_time.rl"
		begin  day := !_int end;
	()
	| 4 ->
# 36 "date_time.rl"
		begin  _int := !_int * 10 + (Char.code data.[p.contents] - Char.code '0') end;
# 32 "date_time.rl"
		begin  hour := !_int end;
	()
	| 5 ->
# 36 "date_time.rl"
		begin  _int := !_int * 10 + (Char.code data.[p.contents] - Char.code '0') end;
# 33 "date_time.rl"
		begin  min := !_int end;
	()
	| 6 ->
# 36 "date_time.rl"
		begin  _int := !_int * 10 + (Char.code data.[p.contents] - Char.code '0') end;
# 34 "date_time.rl"
		begin  sec := !_int end;
	()
	| 12 ->
# 36 "date_time.rl"
		begin  _int := !_int * 10 + (Char.code data.[p.contents] - Char.code '0') end;
# 55 "xensource.rl"
		begin  
    thread_id := !_int;
    date := Some (Date.make ~day:!day ~month:!month ~year:2012 ~hours:!hour ~minutes:!min ~seconds:!sec ~ms:0 ~line: (match log_counter with Some l -> l | None -> 0))
   end;
	()
	| 3 ->
# 35 "date_time.rl"
		begin  _int := 0  end;
# 36 "date_time.rl"
		begin  _int := !_int * 10 + (Char.code data.[p.contents] - Char.code '0') end;
# 31 "date_time.rl"
		begin  day := !_int end;
	()
	| 11 ->
# 35 "date_time.rl"
		begin  _int := 0  end;
# 36 "date_time.rl"
		begin  _int := !_int * 10 + (Char.code data.[p.contents] - Char.code '0') end;
# 55 "xensource.rl"
		begin  
    thread_id := !_int;
    date := Some (Date.make ~day:!day ~month:!month ~year:2012 ~hours:!hour ~minutes:!min ~seconds:!sec ~ms:0 ~line: (match log_counter with Some l -> l | None -> 0))
   end;
	()
# 565 "xensource.ml"
		| _ -> ()
	with Goto_again -> () end;

	do_again ()
	and do_again () =
	match cs.contents with
	| 0 -> do_out ()
	| _ ->
	p.contents <- p.contents + 1;
	if p.contents <> pe.contents then
		do_resume ()
	else do_test_eof ()
and do_test_eof () =
	if p.contents = eof.contents then
	begin try
	begin match _xensource_eof_actions.(cs.contents) with
	| 33 ->
# 76 "xensource.rl"
		begin  message := String.sub data !p (!pe - !p); begin p.contents <- p.contents + 1; if true then raise Goto_out end  end;
	()
# 586 "xensource.ml"
		| _ -> ()
	end
	with Goto_again -> do_again ()
	| Goto_eof_trans -> do_eof_trans () end

	and do_out () = ()
	in do_start ()
	end;

# 103 "xensource.rl"

    with Goto_out -> cs := xensource_first_final | _ -> ();
  end;

  line_num := !line_num + 1;
	if !cs < xensource_first_final then
    fail "xensource: line %d cs %d < %d" !line_num !cs xensource_first_final;

  if !empty_log_line then begin
    None
  end else begin
    let d = match !date with None -> fail "Date not created"
      | Some v -> v in
    let log = (d, !host, !level, !thread_info, !thread_id, !task_ref, !task_name, !key, !message) in
    Some log
  end
  ;;
(*
    390 string [squeezed]
     22 string [v6d]
  62613 string [xapi]
    713 string [xcp-networkd]
   1899 string [xcp-rrdd]
   5670 string [xenopsd]
*)

