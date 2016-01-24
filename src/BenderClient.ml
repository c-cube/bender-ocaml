
(* This file is free software, part of bender-ocaml. See file "license" for more details. *)

(** {1 Client} *)

type 'a or_error = [`Ok of 'a | `Error of string]

exception Error of string

(** Connection to bender *)
type t = {
  push: Nanomsg.socket;
  pull: Nanomsg.socket;
}

let nanomsg_err_to_str (s1,s2) =
  "nanomsg error: " ^ s1 ^ ", " ^ s2

let fail_ e = raise (Error e)

let connect
?(push_path="/tmp/plugin2bender.ipc")
?(pull_path="/tmp/bender2plugin.ipc")
() =
  let open CCError.Infix in
  let res =
    Nanomsg.socket Nanomsg.Push >>= fun push ->
    Nanomsg.socket Nanomsg.Sub >>= fun pull ->
    Nanomsg.connect push (`Ipc push_path) >>= fun _ ->
    Nanomsg.connect pull (`Ipc pull_path) >>= fun _ ->
    Nanomsg.subscribe pull "" >|= fun _ ->
    let c = { push; pull; } in
    c
  in
  CCError.map_err nanomsg_err_to_str res

let connect_exn ?push_path ?pull_path () =
  match connect ?push_path ?pull_path () with
  | `Ok c -> c
  | `Error e -> fail_ e

type chan = string [@@deriving yojson, show]
type user = string [@@deriving yojson, show]

type irc_end_point =
  | Chan of chan * user
  | User of user
  [@@deriving yojson, show]

(** Command sent to the server *)
type command =
  | Privmsg of irc_end_point * string
  | Join of chan
  | Part of chan
  | Reconnect
  | Exit
  [@@deriving yojson, show]

(* TODO: deserialize properly
   {"variant":"Privmsg","fields":[{"variant":"User","fields":["companion_cube"]},"test"]}
   {"variant":"Privmsg","fields":[{"variant":"Chan","fields":["#foo","companion_cube"]},"bar"]}
   *)

let send t cmd =
  let j = command_to_yojson cmd in
  let s = Yojson.Safe.to_string j in
  match Nanomsg.send_string t.push s with
  | `Ok () -> ()
  | `Error e -> fail_ (nanomsg_err_to_str e)

type event =
  | E_privmsg of irc_end_point * string
  | E_joined of chan
  [@@deriving yojson, show]

let receive t =
  let open CCError.Infix in
  CCError.map_err nanomsg_err_to_str (Nanomsg.recv_string t.pull) >>= fun s ->
  Printf.eprintf "received JSON %s\n%!" s;
  ( try `Ok (Yojson.Safe.from_string s)
    with e -> `Error (Printexc.to_string e)
  ) >>= fun j ->
  event_of_yojson j

let receive_exn t = match receive t with
  | `Ok x -> x
  | `Error e -> fail_ e

let rec loop t ~f = match receive t with
  | `Ok e ->
      f e;
      loop t ~f
  | `Error _ as e -> e

let loop_exn t ~f = match loop t ~f with
  | `Ok () -> ()
  | `Error e -> fail_ e

