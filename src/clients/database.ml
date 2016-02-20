
(* This file is free software, part of bender-ocaml. See file "license" for more details. *)

(** {1 Dabatase of replies} *)

module C = BenderClient
module D = Sqlite3.Data

type 'a gen = unit -> 'a option

type t = {
  client: C.t;
  db: Sqlite3.db; (* DB handle *)
}

exception RcError of Sqlite3.Rc.t

(** {2 IRC handling} *)

(* convert a statement into a list of answers *)
let stmt_to_list stmt =
  let rec aux acc = match Sqlite3.step stmt with
    | Sqlite3.Rc.DONE -> acc
    | Sqlite3.Rc.ROW ->
        let row = Sqlite3.row_data stmt in
        aux (row :: acc)
    | rc -> raise (RcError rc)
  in
  aux [] |> List.rev

(* execute statement parametrized by the array of arguments *)
let exec_a db stmt a =
  let stmt = Sqlite3.prepare db stmt in
  Array.iteri (fun i x -> ignore (Sqlite3.bind stmt (i+1) x)) a;
  ignore (Sqlite3.step stmt);
  ()

(* execute statement with 1 param, return rows *)
let exec1 db stmt x : _ list =
  let stmt = Sqlite3.prepare db stmt in
  ignore (Sqlite3.bind stmt 1 x);
  stmt_to_list stmt

(* pick element of list at random *)
let pick_list = function
  | [] -> None
  | l ->
      let i = Random.int (List.length l) in
      Some (List.nth l i)

let norm_key k = String.trim k |> String.lowercase

(* search for replies to [msg] and send them to [reply_to] *)
let search_for_replies t ~reply_to msg =
  let msg = norm_key msg in
  (* find corresponding replies *)
  let l = exec1 t.db "select reply from phrases where quote=?" (D.TEXT msg) in
  match pick_list l with
    | None -> ()
    | Some [| D.TEXT rep |] ->
        Logs.debug (fun k->k "picked reply %s to %s" msg rep);
        C.send t.client (C.Privmsg (reply_to, rep))
    | Some _ -> assert false

let mk_pair x y = x,y

(* if [msg] is a request for updating the DB, do it *)
let update t ~reply_to msg =
  let reply msg = C.send t.client (C.Privmsg (reply_to, msg)) in
  let replyf msg = Printf.ksprintf reply msg in
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
          exec_a t.db "insert into phrases values (?, ?); "
            [| D.TEXT quote; D.TEXT reply |];
          replyf "added `%s` -> `%s`" quote reply
      | "remove" ->
          let arg = norm_key arg in
          Logs.info (fun k->k "remove facts for `%s`" arg);
          exec_a t.db "delete from phrases where quote=?" [| D.TEXT arg |];
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
      (quote text constraint c1 not null, \
       reply text constraint c2 not null);  "
  in ()

(** {2 Main} *)

let db_ = ref "db"

let opts =
  [ "--db", Arg.Set_string db_, " DB file"
  ] |> List.sort Pervasives.compare |> Arg.align ?limit:None

(* TODO: remove once Logs provides a default reporter *)
let reporter ppf =
  let report _src level ~over k msgf =
    let k _ = over(); k() in
    let with_stamp h k ppf fmt =
      Format.kfprintf k ppf ("@[%a " ^^ fmt ^^ "@]@.") Logs.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags:_ fmt -> with_stamp header k ppf fmt
  in
  { Logs.report = report }

let () =
  Arg.parse opts (fun _ -> ()) "database [options]";
  Logs.set_reporter (reporter Format.std_formatter);
  Logs.set_level (Some Logs.Debug);
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
