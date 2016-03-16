
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
        let res = c.cur in
        let next = next_ c.stmt in
        c.cur <- next;
        if next = None then check_ret (Sqlite3.finalize c.stmt);
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

let finally_ ~h ~f x =
  try
    let y = f x in
    h();
    y
  with e ->
    h();
    raise e

let with_stmt db str ~f =
  let stmt = Sqlite3.prepare db str in
  finally_
    ~h:(fun () -> Sqlite3.finalize stmt |> check_ret)
    ~f stmt

(* execute statement, return cursor *)
let exec db str ~f =
  with_stmt db str
    ~f:(fun stmt ->
      assert (Sqlite3.bind_parameter_count stmt = 0);
      f (Cursor.make stmt))

(* execute statement parametrized by the array of arguments *)
let exec_a db str a ~f =
  with_stmt db str
    ~f:(fun stmt ->
      if Sqlite3.bind_parameter_count stmt <> Array.length a
        then invalid_arg
          (Format.sprintf "exec_a: wrong number of arguments, expected %d, got %d"
            (Sqlite3.bind_parameter_count stmt) (Array.length a));
        Array.iteri (fun i x -> ignore (Sqlite3.bind stmt (i+1) x)) a;
        f (Cursor.make stmt))

(* execute statement with 1 param, return rows *)
let exec1 db str x : _ list =
  exec_a db str [| x |] ~f:Cursor.to_list
