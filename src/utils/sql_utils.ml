
(* This file is free software, part of bender-ocaml. See file "license" for more details. *)

(** {1 Utils for SQLite} *)

module D = Sqlite3.Data

exception RcError of Sqlite3.Rc.t

let () = Printexc.register_printer
  (function RcError rc -> Some ("sqlite error: " ^ Sqlite3.Rc.to_string rc)
  | _ -> None)

let check_ret = function
  | Sqlite3.Rc.DONE
  | Sqlite3.Rc.OK -> ()
  | rc -> raise (RcError rc)

module Cursor = struct
  type t = {
    stmt: Sqlite3.stmt;
    mutable cur: D.t array option;
  }

  let next_ stmt = match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> None
      | Sqlite3.Rc.ROW ->
          let row = Sqlite3.row_data stmt in
          Some row
      | rc -> raise (RcError rc)

  let make stmt =
    { stmt;
      cur = next_ stmt;
    }

  let close c = check_ret (Sqlite3.finalize c.stmt)

  (* next value in the cursor *)
  let next c = match c.cur with
    | None -> None
    | Some _ ->
        let res = next_ c.stmt in
        c.cur <- res;
        if res = None then check_ret (Sqlite3.finalize c.stmt);
        res

  let junk c = ignore (next c)

  let head_exn c = match c.cur with
    | None -> failwith "empty cursor"
    | Some d -> d

  let rec iter ~f c = match c.cur with
    | None -> ()
    | Some res -> f res; junk c; iter ~f c

  (* convert a cursor into a list of answers *)
  let to_list_rev
    : t -> _ list
    = fun c ->
      let rec aux acc c = match next c with
        | None -> acc
        | Some d -> aux (d::acc) c
      in
      aux [] c

  let to_list c = List.rev (to_list_rev c)
end

(* execute statement, return cursor *)
let exec db stmt : Cursor.t =
  let stmt = Sqlite3.prepare db stmt in
  assert (Sqlite3.bind_parameter_count stmt = 0);
  Cursor.make stmt

(* execute statement parametrized by the array of arguments *)
let exec_a db stmt a : Cursor.t =
  let stmt = Sqlite3.prepare db stmt in
  if Sqlite3.bind_parameter_count stmt <> Array.length a
    then invalid_arg
      (Format.sprintf "exec_a: wrong number of arguments, expected %d, got %d"
        (Sqlite3.bind_parameter_count stmt) (Array.length a));
  Array.iteri (fun i x -> ignore (Sqlite3.bind stmt (i+1) x)) a;
  Cursor.make stmt

(* execute statement with 1 param, return rows *)
let exec1 db stmt x : _ list =
  let stmt = Sqlite3.prepare db stmt in
  ignore (Sqlite3.bind stmt 1 x);
  let c = Cursor.make stmt in
  Cursor.to_list c
