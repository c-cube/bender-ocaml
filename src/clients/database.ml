
(* This file is free software, part of bender-ocaml. See file "license" for more details. *)

(** {1 Dabatase of replies} *)

module C = BenderClient
module D = Sqlite3.Data
module DU = Sql_utils

type 'a gen = unit -> 'a option

type t = {
  client: C.t;
  db: Sqlite3.db; (* DB handle *)
}

(** {2 IRC handling} *)

(* pick element of list at random *)
let pick_list = function
  | [] -> None
  | l ->
      let i = Random.int (List.length l) in
      Some (List.nth l i)

let norm_key k = String.trim k |> String.lowercase

let chan_of_ep = function
  | C.Chan (c,_) -> c
  | C.User u -> u

(* search for replies to [msg] and send them to [reply_to] *)
let search_for_replies t ~reply_to msg =
  let msg = norm_key msg in
  let chan = chan_of_ep reply_to in
  (* find corresponding replies *)
  let l =
    DU.exec_a t.db "SELECT reply FROM phrases WHERE quote=? and chan=?"
      [| D.TEXT msg; D.TEXT chan |]
    |> DU.Cursor.to_list_rev
  in
  match pick_list l with
    | None -> ()
    | Some [| D.TEXT rep |] ->
        Logs.debug (fun k->k "picked reply %s to %s" msg rep);
        C.privmsg t.client reply_to rep
    | Some _ -> assert false

let mk_pair x y = x,y

(* if [msg] is a request for updating the DB, do it *)
let update t ~reply_to msg =
  let reply msg = C.privmsg t.client reply_to msg in
  let replyf msg = Printf.ksprintf reply msg in
  let chan = chan_of_ep reply_to in
  try
    if msg<>"" && msg.[0] = '!'
    then
      let cmd, arg = Scanf.sscanf msg "!%s@ %s" mk_pair in
      match cmd with
      | "help" ->
          begin match String.trim arg with
          | "" -> reply "commands: !add !help !remove"
          | "add" | "!add" -> reply "usage: !add key|reply"
          | "remove" | "!remove" -> reply "usage: !remove key"
          | _ -> replyf "cannot find help for command `%s`" arg
          end
      | "add" ->
          let quote, reply = Scanf.sscanf arg "%s@|%s" mk_pair in
          let quote = norm_key quote and reply = String.trim reply in
          Logs.info (fun k->k "add fact `%s` -> `%s`" quote reply);
          DU.exec_a t.db "INSERT INTO phrases VALUES (?, ?, ?); "
            [| D.TEXT quote; D.TEXT reply; D.TEXT chan |]
            |> DU.Cursor.close;
          replyf "added `%s` -> `%s`" quote reply
      | "remove" ->
          let arg = norm_key arg in
          Logs.info (fun k->k "remove facts for `%s`" arg);
          DU.exec_a t.db "DELETE FROM phrases WHERE quote=?" [| D.TEXT arg |]
            |> DU.Cursor.close;
          let n = Sqlite3.changes t.db in
          replyf "removed %d rules for `%s`" n arg
      | x ->
          Logs.err (fun k->k "unknown command %s" x);
          replyf "unknown command `%s`" cmd
  with e ->
    Logs.err
      (fun k->k "error reading command %s: %s" msg (Printexc.to_string e))

let handle_event : t -> C.event -> unit
  = fun t e -> match e with
  | C.E_joined _ -> ()
  | C.E_privmsg (ep, msg) ->
      search_for_replies t ~reply_to:ep msg;
      update t ~reply_to:ep msg;
      ()

(* declare required tables if needed *)
let init_db db =
  let _ = Sqlite3.exec db
    "CREATE TABLE IF NOT EXISTS phrases \
      (quote text not null, \
       reply text not null,
       chan text not null); \
     CREATE INDEX IF NOT EXISTS ip on phrases (quote); "
  in ()

(** {2 Main} *)

let db_ = ref "db"
let debug_ = ref false

let opts =
  [ "--db", Arg.Set_string db_, " DB file"
  ; "--debug", Arg.Set debug_, " enable debug logs"
  ] |> List.sort Pervasives.compare |> Arg.align ?limit:None

let () =
  Arg.parse opts (fun _ -> ()) "database [options]";
  Logs.set_reporter (Utils.reporter Format.std_formatter);
  Logs.set_level (Some (if !debug_ then Logs.Debug else Logs.Info));
  Logs.info (fun k->k "open DB `%s`" !db_);
  let db = Sqlite3.db_open !db_ in
  init_db db;
  Logs.info (fun k->k "connecting to bender...");
  let c = C.connect_exn () in
  Logs.info (fun k->k "connected");
  let t = { db; client=c; } in
  C.loop_exn c
    ~f:(fun e ->
      Logs.debug (fun k->k "@[<2>receive event@ @[%a@]@]@." C.pp_event e);
      handle_event t e)
