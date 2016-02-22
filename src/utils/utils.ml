
(* This file is free software, part of bender-ocaml. See file "license" for more details. *)

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
