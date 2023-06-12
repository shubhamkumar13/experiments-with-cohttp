module Cohttp_lwt_unix = Cohttp_lwt_unix
module Json = Yojson.Basic
module Cohttp_lwt = Cohttp_lwt
open Lwt.Syntax
open Lwt.Infix

module List = struct
  include List

  let dedup lst =
    let rec aux acc = function
      | [] -> acc
      | hd :: tl -> if List.mem hd acc then aux acc tl else aux (hd :: acc) tl
    in
    List.rev (aux [] lst)
end

let ( // ) a b = a ^ "/" ^ b
let package_path = Sys.getcwd () // "package.json"
let package_json = Json.from_file package_path
let ligo_package_dir = "." // ".ligo" // "source" // "i"
let baseurl = "https://packages.ligolang.org/-/api"
let get_pkg_url pkg = baseurl // pkg
let esy_lock_dir = Sys.getcwd () // "esy.lock"
let underscore_esy = "." // "_esy" // "default"

let toplevel_pkgs package_json =
  Json.Util.member "dependencies" package_json
  |> Json.Util.to_assoc
  |> List.map (fun (k, v) -> (k, Json.to_string v))

let trim s = String.split_on_char '\"' s |> fun lst -> List.nth lst 1

let get_json pkg =
  let* _, body =
    Cohttp_lwt_unix.Client.get (Uri.of_string @@ get_pkg_url pkg)
  in
  let* body = Cohttp_lwt.Body.to_string body in
  Json.from_string body |> Lwt.return

let find_latest_version name =
  let* json = get_json name in
  Json.Util.(
    member "dist-tags" json |> member "latest" |> Json.to_string |> trim)
  |> Lwt.return

let check_format name version =
  if String.contains version '^' || String.contains version '~' then
    find_latest_version name
  else Lwt.return version

let tarball_name url =
  url |> Uri.to_string |> String.trim |> String.split_on_char '/' |> List.rev
  |> List.hd

module De = De
module Gz = Gz
open Lwt.Syntax

type error = UnableToUnzip

let unzip fname =
  let in_fd = Unix.openfile fname [ Lwt_unix.O_RDONLY ] 0 in
  let file_size = (Unix.stat fname).st_size in
  let buffer_len = De.io_buffer_size in
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let r = Buffer.create 0x1000 in
  let p = ref 0 in
  let refill buf =
    let len = min (file_size - !p) buffer_len in
    if len <= 0 then 0
    else
      let bytes = Bytes.create len in
      let len = Unix.read in_fd bytes 0 len in
      Bigstringaf.blit_from_bytes bytes ~src_off:0 buf ~dst_off:0 ~len;
      p := !p + len;
      len
  in
  let flush buf len =
    let str = Bigstringaf.substring buf ~off:0 ~len in
    Buffer.add_string r str
  in
  match Gz.Higher.uncompress ~refill ~flush i o with
  | Ok _ ->
      let bytes = Buffer.to_bytes r in
      let nbytes = Bytes.length bytes in
      let fname = Format.sprintf "%s.tar" (Filename.remove_extension fname) in
      let out_fd = Unix.openfile fname [ Unix.O_CREAT; Unix.O_RDWR ] 0o666 in
      let mbytes = Unix.write out_fd bytes 0 nbytes in
      let () = Unix.close in_fd in
      let () = Unix.close out_fd in
      if nbytes = mbytes then Ok fname else Error UnableToUnzip
  | Error (`Msg _) ->
      let () = Unix.close in_fd in
      Error UnableToUnzip

let mkdir ~perm dir = Unix.mkdir dir perm
let touch f = Unix.openfile f [ Unix.O_CREAT ] 0o666 |> Unix.close

let mkdir_p ~perm dir =
  let rec dirs dir =
    if dir = Filename.dirname dir then []
    else Filename.basename dir :: dirs (Filename.dirname dir)
  in
  let dirs = dirs dir in
  let _ =
    List.fold_right
      (fun dir dirs ->
        let dir =
          if dir = String.empty then dir else Filename.concat dirs dir
        in
        let () =
          try mkdir ~perm dir
          with Unix.(Unix_error ((EEXIST | EISDIR), _, _)) -> ()
        in
        dir)
      dirs String.empty
  in
  ()

let untar ~dest_dir fname =
  let fd = Unix.openfile fname [ Unix.O_RDONLY ] 0 in
  let move f =
    (* Printf.printf "debug\n"; *)
    let f = Filename.concat dest_dir f in
    let f =
      Fpath.to_string @@ Fpath.normalize
      @@
      match Fpath.of_string f with
      | Error _ -> failwith "Normalize fails"
      | Ok f -> f
    in
    let () = mkdir_p ~perm:0o755 (Filename.dirname f) in
    assert (Sys.is_directory (Filename.dirname f));
    let () = touch f in
    f
  in
  let () = Tar_unix.Archive.extract move fd in
  Unix.close fd

let download_response url filename =
  let* body =
    Cohttp_lwt_unix.Client.get url >>= fun (_, body) -> Lwt.return body
  in
  let stream = Cohttp_lwt.Body.to_stream body in
  Lwt_io.with_file ~mode:Lwt_io.output (ligo_package_dir // filename)
    (fun chan -> Lwt_stream.iter_s (Lwt_io.write chan) stream)

let do_request index_json =
  Sys.command @@ "mkdir -p " ^ ligo_package_dir |> fun _ ->
  let tarball_url json =
    Json.Util.(
      member "source" json |> member "source" |> to_list |> filter_string)
    |> List.hd
    |> Str.(split @@ regexp "archive:")
    |> List.hd
    |> Str.(split @@ regexp "#")
    |> List.hd
  in
  let rec aux json_list =
    match json_list with
    | [] -> Lwt.return_unit
    | (name, json) :: rest ->
        let url = tarball_url json |> Uri.of_string in
        let info = Str.(split @@ regexp "@") name in
        let name =
          (List.hd info |> Str.(split @@ regexp "/")) |> fun lst ->
          List.nth lst (List.length lst - 1)
        in
        let version = List.nth info 1 in
        let filename = name ^ "-" ^ version ^ ".tgz" in
        download_response url filename >>= fun _ -> aux rest
  in
  let json_list =
    Json.Util.member "node" index_json
    |> Json.Util.to_assoc |> List.dedup |> List.rev |> List.tl |> List.rev
  in
  aux json_list >>= fun _ ->
  List.iter
    (fun (name, _) ->
      let info = Str.(split @@ regexp "@") name in
      let dirname, name =
        (List.hd info |> Str.(split @@ regexp "/")) |> fun lst ->
        match List.nth_opt lst 1 with
        | None -> (List.nth lst 0, List.nth lst 0)
        | Some name -> ("ligo__s__" ^ name, name)
      in
      let version = List.nth info 1 in
      let dir = ligo_package_dir // (dirname ^ "__" ^ version) in
      Sys.command ("mkdir -p " ^ dir) |> fun _ ->
      let file = ligo_package_dir // (name ^ "-" ^ version ^ ".tgz") in
      unzip file |> Result.map (untar ~dest_dir:dir) |> fun res ->
      match res with
      | Error _ -> failwith "Error while untaring to a dir"
      | Ok _ -> ())
    json_list;
  let files =
    Sys.readdir ligo_package_dir
    |> Array.to_list
    |> List.filter (fun file ->
           not @@ Sys.is_directory (ligo_package_dir // file))
  in
  List.iter (fun file -> Sys.remove (ligo_package_dir // file)) files
  |> Lwt.return

let _print_list_list_lwt monster =
  let* lst_lst = monster in
  List.iter
    (fun lst ->
      List.iter (fun (pkg, ver) -> Printf.printf "%s -> %s" pkg ver) lst;
      print_newline ())
    lst_lst
  |> Lwt.return

let get_dep_json_list package_json =
  let dependencies = Json.Util.member "dependencies" package_json in
  let dependencies =
    match Json.Util.(to_option to_assoc dependencies) with
    | None -> []
    | Some lst ->
        List.map
          (fun (name, version) -> (name, Json.to_string version |> trim))
          lst
  in
  let rec loop dependencies acc =
    match dependencies with
    | [] -> Lwt.return acc
    | (name, version) :: rest ->
        let* version = check_format name version in
        let* json = get_json name in
        let json = Json.Util.(member "versions" json |> member version) in
        let rest =
          match
            Json.Util.(member "dependencies" json |> to_option to_assoc)
          with
          | None -> rest
          | Some lst ->
              rest
              @ List.map
                  (fun (name, version) ->
                    (name, Json.to_string version |> trim))
                  lst
        in
        let acc = json :: acc in
        loop rest acc
  in
  loop dependencies []

let create_index_node json : string * Json.t =
  let name = Json.Util.member "name" json |> Json.to_string |> trim in
  let version = Json.Util.member "version" json |> Json.to_string |> trim in
  let key = name ^ "@" ^ version in
  let id = ("id", `String key) in
  let name = ("name", `String name) in
  let version = ("version", `String version) in
  let shasum =
    Json.Util.(member "dist" json |> member "shasum") |> Json.to_string |> trim
  in
  let tarball =
    Json.Util.(member "dist" json |> member "tarball") |> Json.to_string |> trim
  in
  let source =
    let type_ = "install" in
    let source = "archive:" ^ tarball ^ "#sha1:" ^ shasum in
    ( "source",
      `Assoc [ ("type", `String type_); ("source", `List [ `String source ]) ]
    )
  in
  let overrides = ("overrides", `List []) in
  let dependencies =
    let deps = Json.Util.(member "dependencies" json |> to_option to_assoc) in
    match deps with
    | None -> ("dependencies", `List [])
    | Some lst ->
        let lst =
          List.map
            (fun (key, value) ->
              `String (key ^ "@" ^ trim @@ Json.to_string value))
            lst
        in
        ("dependencies", `List lst)
  in
  let dev_dependencies =
    let deps =
      Json.Util.(member "devDependencies" json |> to_option to_assoc)
    in
    match deps with
    | None -> ("devDependencies", `List [])
    | Some lst ->
        let lst =
          List.map
            (fun (key, value) ->
              `String (key ^ "@" ^ trim @@ Json.to_string value))
            lst
        in
        ("devDependencies", `List lst)
  in
  let obj =
    `Assoc
      [ id; name; version; source; overrides; dependencies; dev_dependencies ]
  in
  (key, obj)

let create_index_root_node json : string * Json.t =
  let name = Json.Util.(member "name" json) |> Json.to_string |> trim in
  let version = "link-dev:./package.json" in
  let key = name ^ "@" ^ version in
  let id = ("id", `String key) in
  let name = ("name", `String name) in
  let version = ("version", `String version) in
  let source =
    let type_ = ("type", `String "link-dev") in
    let path = ("path", `String ".") in
    let manifest = ("manifest", `String "package.json") in
    ("source", `Assoc [ type_; path; manifest ])
  in
  let overrides = ("overrides", `List []) in
  let dependencies =
    let deps = Json.Util.(member "dependencies" json |> to_option to_assoc) in
    match deps with
    | None -> ("dependencies", `List [])
    | Some lst ->
        let lst =
          List.map
            (fun (key, value) ->
              `String (key ^ "@" ^ trim @@ Json.to_string value))
            lst
        in
        ("dependencies", `List lst)
  in
  let dev_dependencies =
    let deps =
      Json.Util.(member "devDependencies" json |> to_option to_assoc)
    in
    match deps with
    | None -> ("devDependencies", `List [])
    | Some lst ->
        let lst =
          List.map
            (fun (key, value) ->
              `String (key ^ "@" ^ trim @@ Json.to_string value))
            lst
        in
        ("devDependencies", `List lst)
  in
  let obj =
    `Assoc
      [ id; name; version; source; overrides; dependencies; dev_dependencies ]
  in
  (key, obj)

let create_index_json dep_json_list : Json.t =
  let rec loop json_list lst =
    match json_list with
    | [] -> List.rev lst
    | json :: rest -> loop rest (create_index_node json :: lst)
  in
  let root_node = create_index_root_node package_json in
  let root =
    let name = Json.Util.member "name" package_json |> Json.to_string |> trim in
    ("root", `String (name ^ "@" ^ "link-dev:./package.json"))
  in
  let node = ("node", `Assoc (loop dep_json_list [] @ [ root_node ])) in
  let index_json = `Assoc [ root; node ] in
  Sys.command @@ "mkdir -p " ^ esy_lock_dir |> fun _ ->
  Json.to_file (esy_lock_dir // "index.json") index_json;
  index_json

(* Do this after downloading tarball *)
let create_installation_json () : Json.t =
  let files =
    Sys.readdir (Sys.getcwd () // ".ligo" // "source" // "i") |> Array.to_list
  in
  let create_json files : Json.t =
    match files with
    | [] -> failwith "The directory .ligo/source/i is empty"
    | files ->
        let get_key file =
          let lst = Str.(split @@ regexp "__") file in
          if String.equal (List.hd lst) "ligo" then
            ("@ligo" // List.nth lst 2) ^ "@" ^ List.nth lst 3
          else List.nth lst 0 ^ "@" ^ List.nth lst 1
        in
        let keys = List.map get_key files in
        let pairs =
          List.map2 (fun key value -> (key, `String value)) keys files
        in
        `Assoc pairs
  in
  let installation_json = create_json files in
  Sys.command @@ "mkdir -p " ^ underscore_esy |> fun _ ->
  Json.to_file (underscore_esy // "installation.json") installation_json;
  installation_json

let get_name_ver dep_json =
  let name = Json.Util.member "name" dep_json |> Json.to_string |> trim in
  let version = Json.Util.member "version" dep_json |> Json.to_string |> trim in
  (name, version)

let main package_json =
  let* dep_json_list = get_dep_json_list package_json in
  let index_json = create_index_json dep_json_list in
  do_request index_json >>= fun _ ->
  let installation_json = create_installation_json () in
  Lwt.return_unit

let _ =
  (* Printf.printf "%s\n" @@ Json.to_string package_json; *)
  Lwt_main.run @@ main package_json
