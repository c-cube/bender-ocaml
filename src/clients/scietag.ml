
(* This file is free software, part of bender-ocaml. See file "license" for more details. *)

(** {1 Bender Plugin} *)

module C = BenderClient
module TU = Scietag_db.Tagged_URL

type t = {
  db: Scietag_db.t;
  client: BenderClient.t;
}

let create ~db =
  let client = BenderClient.connect_exn () in
  { db=Scietag_db.create db; client; }

(* look for URLs in msg (coming from endpoint [ep]) *)
let scan_for_urls db ep msg =
  let author, chan = match ep with
    | C.Chan (c,u) -> u, c
    | C.User u -> u, ""
  in
  match TU.parse ~author ~chan msg  with
    | None -> ()
    | Some u ->
        Logs.info (fun k->k "found URL @[%a@]" TU.print u);
        Scietag_db.add_tagged_url db u

let mk_pair x y = x,y

let limit = 3

let interpret_cmd t ep msg =
  if msg<>"" && msg.[0] = '!'
  then try
    let cmd, arg = Scanf.sscanf msg "!%s@ %s" mk_pair in
    match cmd with
    | "help" ->
        let msg =
          "scietag: !search_url <url> | !help | \
          !search_tag <tag> | search_author <author> | list_authors" in
        C.privmsg t.client ep msg
    | "list_authors" ->
        let l = Scietag_db.list_authors ~limit:30L t.db in
        let msg = CCFormat.sprintf "@[%a@]"
          CCFormat.(list ~start:"" ~stop:"" ~sep:" " string) l in
        C.privmsg t.client ep msg
    | "search_author" ->
        let author = String.trim arg in
        let l = Scietag_db.find_by_author ~limit:3L t.db author in
        List.iter
          (fun (chan,url,tags) ->
            let msg = CCFormat.sprintf "on %s with tags %a: %s"
              chan TU.pp_tags tags url in
            C.privmsg t.client ep msg)
          l
    | "search_url" ->
        let url = String.trim arg in
        let l = Scietag_db.find_by_url ~limit:3L t.db url in
        List.iter
          (fun (chan,author,tags) ->
            let msg = CCFormat.sprintf "on %s by %s with tags %a"
              chan author TU.pp_tags tags in
            C.privmsg t.client ep msg)
          l
    | "search_tag" ->
        let tag = Scietag_db.norm_tag arg in
        let l = Scietag_db.find_by_tag ~limit:3L t.db tag in
        List.iter
          (fun (author,chan,url) ->
            let msg = CCFormat.sprintf "on %s by %s: %s "
              chan author url in
            C.privmsg t.client ep msg)
          l
    | _ ->
        C.privmsg t.client ep (CCFormat.sprintf "unknown command `%s`" cmd)
  with Scanf.Scan_failure _ | End_of_file -> ()

let handle_event t e = match e with
  | C.E_joined _ -> ()
  | C.E_privmsg (ep, msg) ->
      scan_for_urls t.db ep msg;
      interpret_cmd t ep msg;
      ()


(** {2 CLI} *)

let db_ = ref "db"
let debug_ = ref false

let opts =
  [ "--db", Arg.Set_string db_, " DB file"
  ; "--debug", Arg.Set debug_, " enable debug logs"
  ] |> List.sort Pervasives.compare |> Arg.align ?limit:None

let () =
  Arg.parse opts (fun _ -> ()) "scietag [options]";
  Logs.set_reporter (Utils.reporter Format.std_formatter);
  Logs.set_level (Some (if !debug_ then Logs.Debug else Logs.Info));
  let t = create ~db:!db_ in
  Logs.info (fun k->k "connected");
  C.loop_exn t.client
    ~f:(fun e ->
      Logs.debug (fun k->k "@[<2>receive event@ @[%a@]@]@." C.pp_event e);
      handle_event t e)
