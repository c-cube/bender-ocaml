
(* This file is free software, part of bender-ocaml. See file "license" for more details. *)

(** {1 Client} *)

type 'a or_error = [`Ok of 'a | `Error of string]
type json = Yojson.Safe.json

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
    Nanomsg.subscribe pull "" >>= fun _ ->
    Nanomsg.connect push (`Ipc push_path) >>= fun _ ->
    Nanomsg.connect pull (`Ipc pull_path) >|= fun _ ->
    let c = { push; pull; } in
    c
  in
  CCError.map_err nanomsg_err_to_str res

let connect_exn ?push_path ?pull_path () =
  match connect ?push_path ?pull_path () with
  | `Ok c -> c
  | `Error e -> fail_ e

(* Helpers for JSON *)
module JS = struct
  exception Err of string
  let err_ msg = raise (Err msg)
  let errf_ msg = Format.ksprintf err_ msg

  let l_assoc_ s l : json =
    try List.assoc s l with Not_found -> errf_ "key %s not found" s
  let assoc_ s j : json = match j with
    | `Assoc l -> l_assoc_ s l
    | _ -> err_ "expected dictionary"

  (* match a variant encoded with rust's conventions *)
  let read_var : json -> string * json list
  = function
    | `Assoc l ->
        begin match l_assoc_ "variant" l, l_assoc_ "fields" l with
        | `String s, `List l -> s, l
        | _ -> err_ "expected variant"
        end
    | _ -> err_ "expected variant"

  (* make a variant, with rust's conventions *)
  let mk_var v args =
    `Assoc ["variant", `String v; "fields", `List args]

  let guard f j =
    try `Ok (f j)
    with Err msg ->
      let msg = Printf.sprintf "error: %s (json %s)"
        msg (Yojson.Safe.to_string j) in
      `Error msg
end

type chan = string [@@deriving show]
type user = string [@@deriving show]

type irc_end_point =
  | Chan of chan * user
  | User of user
  [@@deriving show]

let iep_to_yojson iep : json = match iep with
  | Chan (c,u) -> JS.mk_var "Chan" [`String c; `String u]
  | User u -> JS.mk_var "User" [`String u]

let iep_of_yojson_exn : json -> irc_end_point
  = fun j -> match JS.read_var j with
  | "Chan", [`String c; `String u] -> Chan (c,u)
  | "User", [`String u] -> User u
  | _ -> JS.err_ "expected irc_end_point"

let iep_of_yojson = JS.guard iep_of_yojson_exn

let chan_of_ep = function
  | Chan (c,_) -> c
  | User u -> u

(** Command sent to the server *)
type command =
  | Privmsg of irc_end_point * string
  | Join of chan
  | Part of chan
  | Reconnect
  | Exit
  [@@deriving show]

let command_to_yojson c : json = match c with
  | Privmsg (iep,msg) -> JS.mk_var "Privmsg" [iep_to_yojson iep; `String msg]
  | Join c -> JS.mk_var "Join" [`String c]
  | Part c -> JS.mk_var "Part" [`String c]
  | Reconnect -> JS.mk_var "Reconnect" []
  | Exit -> JS.mk_var "Exit" []

let command_of_yojson_exn : json -> command
  = fun j -> match JS.read_var j with
  | "Privmsg", [iep; `String msg] -> Privmsg (iep_of_yojson_exn iep, msg)
  | "Join", [`String c] -> Join c
  | "Part", [`String c] -> Part c
  | "Reconnect", [] -> Reconnect
  | "Exit", [] -> Exit
  | _ -> JS.err_ "expected command"

let command_of_yojson = JS.guard command_of_yojson_exn

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

let join t c = send t (Join c)
let part t c = send t (Part c)
let reconnect t = send t Reconnect
let exit t = send t Exit
let privmsg t ep msg = send t (Privmsg (ep, msg))

type event =
  | E_privmsg of irc_end_point * string
  | E_joined of chan
  [@@deriving show]

let event_to_yojson e : json = match e with
  | E_privmsg (iep, s) -> JS.mk_var "Privmsg" [iep_to_yojson iep; `String s]
  | E_joined c -> JS.mk_var "Joined" [`String c]

let event_of_yojson_exn : json -> event
  = fun j -> match JS.read_var j with
  | "Privmsg", [iep; `String s] -> E_privmsg (iep_of_yojson_exn iep, s)
  | "Joined", [`String c] -> E_joined c
  | _ -> JS.err_ "expected event"

let event_of_yojson = JS.guard event_of_yojson_exn

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

