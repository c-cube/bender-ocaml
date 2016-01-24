
(* This file is free software, part of bender-ocaml. See file "license" for more details. *)

(** {1 Trivial "echo" server} *)

module C = BenderClient

let () =
  Format.printf "connecting...@.";
  let c = C.connect_exn () in
  Format.printf "connected@.";
  C.loop_exn c
    ~f:(fun e ->
      Format.printf "receive event %a@." C.pp_event e)

