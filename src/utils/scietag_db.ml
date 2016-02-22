
(* This file is free software, part of bender-ocaml. See file "license" for more details. *)

(** {1 DB interface for Scietag} *)

module D = Sqlite3.Data
module DU = Sql_utils

type t = Sqlite3.db

let re_url = Re_posix.compile_pat "(https?|ftp)://[^ \t/$.?#].[^ \t]+"

let re_tag = Re_posix.compile_pat "\\\\<#([^# ]+)"

type author = string
type tag = string
type chan = string
type url = string

(** {2 Representation of tagged URLs *)
module Tagged_URL = struct
  type t = {
    url : url;
    tags : tag list;
    chan : chan;
    author : author;
  }

  let url x = x.url
  let tags x = x.tags
  let author x = x.author
  let chan x = x.chan

  let add_tags t l =
    let tags = CCList.Set.union l t.tags in
    { t with tags }

  (* parse a tagged url from a IRC message body *)
  let parse ~author ~chan s =
    try
      let url = Re.get (Re.exec re_url s) 0 in
      let tags = Re.all re_tag s |> List.map (fun s -> Re.get s 1) in
      Some {url; tags; author; chan;}
    with Not_found ->
      None

  let pp_tag fmt t = Format.fprintf fmt "#%s" t
  let pp_tags = CCFormat.list ~sep:" " pp_tag

  let print fmt t =
    Format.fprintf fmt "@[<2>\"%s\" %a@ by %s on %s@]" t.url
      (CCFormat.list ~sep:" " pp_tag) t.tags t.author t.chan
end

let create file : t =
  let db = Sqlite3.db_open file in
  Logs.debug (fun k->k "opened DB file %s" file);
  Sqlite3.exec db
    "CREATE TABLE IF NOT EXISTS scietag_urls
      ( author TEXT NOT NULL
      , chan TEXT NOT NULL
      , url TEXT NOT NULL
      , id INTEGER PRIMARY KEY
      );
     CREATE INDEX IF NOT EXISTS scietag_tags_idx_author
      ON scietag_urls ( author );
     CREATE TABLE IF NOT EXISTS scietag_tags
       ( tag TEXT NOT NULL
       , url INTEGER
       , FOREIGN KEY (url) REFERENCES scietag_urls(id)
       ) ;
     CREATE INDEX IF NOT EXISTS scietag_tags_idx_tag
      ON scietag_tags ( tag );
    "
    |> DU.check_ret;
  db

let close db = ignore (Sqlite3.db_close db)

let with_file file ~f =
  let db = create file in
  try
    let x = f db in
    close db;
    x
  with e ->
    close db;
    raise e

let add_tagged_url db t_url =
  let open Tagged_URL in
  DU.exec_a db
    "INSERT
      INTO scietag_urls ( author, chan, url, id )
      VALUES            ( ?, ?, ?, NULL);
    "
    [| D.TEXT t_url.author; D.TEXT t_url.chan; D.TEXT t_url.url |]
    |> DU.Cursor.close;
  (* get last ID *)
  let id =
    DU.exec db "SELECT last_insert_rowid();"
    |> DU.Cursor.head_exn
    |> (function
        | [| D.INT i |] -> i
        | _ -> assert false)
  in
  List.iter
    (fun tag ->
      DU.exec_a db
        "INSERT INTO scietags_tags ( tag, url ) VALUES ( ?, ? ); "
        [| D.TEXT tag; D.INT id |]
      |> DU.Cursor.close)
    t_url.tags;
  ()

(* normalize tag *)
let norm_tag t = t |> String.trim |> String.lowercase

(* return the sequence of URLs that contain this tag *)
let find_by_tag ~limit db tag : (author * chan * url) list =
  let tag = norm_tag tag in
  let c = DU.exec_a db
    "SELECT author, chan, u.url
      FROM scietag_urls u JOIN scietag_tags t ON u.id = t.url
      WHERE t.tag = ?
      LIMIT ? ; "
    [| D.TEXT tag; D.INT limit |]
  in
  DU.Cursor.to_list_rev c
    |> List.rev_map
      (function
        | [| D.TEXT author; D.TEXT chan; D.TEXT url |] -> author,chan,url
        | _ -> assert false)

let find_by_author ~limit db a : (chan * url * tag list) list =
  let l = DU.exec_a db
    "ELECT chan, url, id
      FROM scietag_urls
      WHERE author = ?
      LIMIT ? ; "
    [| D.TEXT a; D.INT limit |]
    |> DU.Cursor.to_list_rev
  in
  List.rev_map
    (function
      | [| D.TEXT chan; D.TEXT url; id |] ->
          let tags = DU.exec_a db
            "SELECT tag from scietag_tags WHERE url = ?" [| id |]
            |> DU.Cursor.to_list_rev
            |> List.rev_map
              (function
                | [| D.TEXT tag |] -> tag
                | _ -> assert false)
          in
          chan, url, tags
      | _ -> assert false)
    l

let find_by_url ~limit db u : (chan * author * tag list) list =
  let l = DU.exec_a db
    "SELECT chan, author, id
      FROM scietag_urls
      WHERE url = ?
      LIMIT ? ; "
    [| D.TEXT u; D.INT limit |]
    |> DU.Cursor.to_list_rev
  in
  List.rev_map
    (function
      | [| D.TEXT chan; D.TEXT author; id |] ->
          let tags = DU.exec_a db
            "SELECT tag from scietag_tags WHERE url = ?" [| id |]
            |> DU.Cursor.to_list_rev
            |> List.rev_map
              (function
                | [| D.TEXT tag |] -> tag
                | _ -> assert false)
          in
          chan, author, tags
      | _ -> assert false)
    l

let list_authors ~limit db : author list =
  DU.exec_a db "SELECT DISTINCT author FROM scietag_urls LIMIT ? ;"
    [| D.INT limit |]
    |> DU.Cursor.to_list_rev
    |> List.rev_map
      (function
        | [| D.TEXT a |] -> a
        | _ -> assert false)

