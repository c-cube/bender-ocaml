
(* This file is free software, part of bender-ocaml. See file "license" for more details. *)

(** {1 Client} *)

type 'a or_error = [`Ok of 'a | `Error of string]
type json = Yojson.Safe.json

type t
(** Connection to bender *)

exception Error of string

val connect :
  ?push_path:string ->
  ?pull_path:string ->
  unit ->
  t or_error
(** [connect ()] connects to a running instance of Bender
    @param push_path the file to push commands
    @param pull_path the file to receive events *)

val connect_exn : ?push_path:string -> ?pull_path:string -> unit -> t
(** Unsafe version of {!connect} *)

type chan = string
type user = string

type irc_end_point =
  | Chan of chan * user
  | User of user
  [@@deriving show]

(** Command sent to the server *)
type command =
  | Privmsg of irc_end_point * string
  | Join of chan
  | Part of chan
  | Reconnect
  | Exit
  [@@deriving show]

val command_to_yojson : command -> json
val command_of_yojson : json -> command or_error

val send : t -> command -> unit
(** Send a command to the server
    @raise Error on failure *)

val join : t -> chan -> unit
val part : t -> chan -> unit
val reconnect : t -> unit
val exit : t -> unit
val privmsg : t -> irc_end_point -> string -> unit

(** Event received from the server *)
type event =
  | E_privmsg of irc_end_point * string
  | E_joined of chan
  [@@deriving show]

val event_to_yojson : event -> json
val event_of_yojson : json -> event or_error

val receive : t -> event or_error
(** Receive next even *)

val receive_exn : t -> event
(** Unsafe version of {!receive}
    @raise Error on failure *)

val loop :
  t ->
  f:(event -> unit) ->
  unit or_error
(** [loop conn ~f] loops on events from [conn]. For each event [e],
    [f e] is called (and might call {!send} or do other things). *)

val loop_exn : t -> f:(event -> unit) -> unit
(** @raise Error on failure *)


