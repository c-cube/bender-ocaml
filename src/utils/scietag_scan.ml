
(* This file is free software, part of bender-ocaml. See file "license" for more details. *)

(** {1 Scan IRC logs} *)

type 'a sequence = ('a -> unit) -> unit

(* Logged message *)
type msg = {
  time:string;
  author:string;
  content:string;
}

(* regex for parsing logs *)
let re = Re_posix.compile_pat "([^:].*:[^:]*:[^:]{2})<.([^>]+)> (.+)$"

let parse_file filename: msg sequence =
  Sequence.IO.lines_of filename
  |> Sequence.fmap
    (fun s ->
      try
        let sub = Re.exec re s in
        let groups = Re.get_all sub in
        Some {time=groups.(1); author=groups.(2); content=groups.(3)}
      with Not_found ->
        None
    )

module TUrl = Scietag_db.Tagged_URL

(* try to infer the channel name from the file name *)
let parse_chan s =
  try Scanf.sscanf s "%s@#%s@/" (fun _ s -> "#" ^ s)
  with Scanf.Scan_failure _ | End_of_file -> ""

(* scan the given file and add links to the DB *)
let scan_file db f =
  let chan = parse_chan f in
  Logs.info (fun k->k "@[<2>parsing file `%s`@ (chan %s)...@]" f chan);
  if Sys.is_directory f
  then ()
  else parse_file f
  |> Sequence.iter
    (fun msg ->
      match TUrl.parse ~author:msg.author ~chan msg.content with
      | None -> ()
      | Some turl ->
          Logs.debug (fun k->k "@[<2>in file `%s`,@ found URL @[%a@]@]" f TUrl.print turl);
          Scietag_db.add_tagged_url db turl
    )


let extrapolate f =
  let buf = Buffer.create 16 in
  Buffer.add_substitute buf Sys.getenv f;
  Buffer.contents buf

let scan_files db files =
  let db_path = extrapolate db in
  Scietag_db.with_file db_path
    ~f:(fun db -> List.iter (scan_file db) files)

(** {2 Main} *)

let files = ref []
let db = ref "db"
let debug_ = ref false
let add_file = CCList.Ref.push files

let options =
  [ "--db", Arg.Set_string db, " set the DB file to use"
  ; "--debug", Arg.Set debug_, " set debug"
  ; "-d", Arg.Set debug_, " alias to --debug"
  ]

let usage = "scietag_scan files"

let () =
  Arg.parse (Arg.align options) add_file usage;
  Logs.set_reporter (Utils.reporter Format.std_formatter);
  Logs.set_level (Some (if !debug_ then Logs.Debug else Logs.Info));
  try
    scan_files !db !files
  with e ->
    let st = Printexc.get_backtrace () in
    print_endline ("error: " ^ Printexc.to_string e);
    print_endline st;
    exit 1
