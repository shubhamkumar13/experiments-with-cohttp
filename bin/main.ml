module Cohttp_lwt_unix = Cohttp_lwt_unix
module Json = Yojson.Basic
module Cohttp_lwt = Cohttp_lwt
open Lwt.Syntax
open Lwt.Infix

let ( // ) a b = a ^ "/" ^ b
let package_path = Sys.getcwd () // "package.json"
let package_json = Json.from_file package_path
let ligo_package_dir = "." // ".ligo" // "source" // "i"
let baseurl = "https://packages.ligolang.org/-/api"
let get_pkg_url pkg = baseurl // pkg

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

let get_tarball_url pkg json =
  let url = baseurl // fst pkg in
  let* version =
    if snd pkg == "" then find_latest_version url else Lwt.return @@ snd pkg
  in
  let tarball_url json =
    Json.Util.(
      member "versions" json |> member version |> member "dist"
      |> member "tarball")
    |> Json.to_string
  in
  tarball_url json |> trim |> Lwt.return

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

let download_response url =
  let* body =
    Cohttp_lwt_unix.Client.get url >>= fun (_, body) -> Lwt.return body
  in
  let stream = Cohttp_lwt.Body.to_stream body in
  Lwt_io.with_file ~mode:Lwt_io.output
    (ligo_package_dir // tarball_name url)
    (fun chan -> Lwt_stream.iter_s (Lwt_io.write chan) stream)

let do_request pkgs =
  Sys.command @@ "mkdir -p " ^ ligo_package_dir |> fun _ ->
  let tarball_url (pkg, ver) json = get_tarball_url (pkg, ver) json in
  let rec aux pkgs =
    match pkgs with
    | [] -> Lwt.return_unit
    | (pkg, ver) :: rest ->
        let* json = get_json pkg in
        let* url =
          tarball_url (pkg, ver) json >>= fun url ->
          Lwt.return @@ Uri.of_string url
        in
        download_response url >>= fun _ -> aux rest
  in
  aux pkgs >>= fun _ ->
  let is_tgz file =
    String.split_on_char '.' file |> List.rev |> List.hd |> ( = ) "tgz"
  in
  let files =
    Sys.readdir ligo_package_dir |> Array.to_list |> List.filter is_tgz
  in
  (* List.iter (fun file -> Printf.printf "%s\n" file) files; *)
  (* Printf.printf "%d\n" @@ List.length files; *)
  List.iter
    (fun file ->
      let pkg_name, version =
        let lst = String.split_on_char '-' file in
        let pkg_name = List.hd lst in
        let version =
          List.tl lst |> List.hd |> String.split_on_char '.' |> fun lst ->
          let major = List.nth lst 0 in
          let minor = List.nth lst 1 in
          let patch = List.nth lst 2 in
          major ^ "." ^ minor ^ "." ^ patch
        in
        (pkg_name, version)
      in
      let dir = ligo_package_dir // ("ligo__s__" ^ pkg_name ^ "__" ^ version) in
      Sys.command ("mkdir -p " ^ dir) |> fun _ ->
      let file = ligo_package_dir // file in
      unzip file |> Result.map (untar ~dest_dir:dir) |> fun res ->
      match res with
      | Error _ -> failwith "Error while untaring to a dir"
      | Ok _ -> ())
    files;
  let files = Sys.readdir ligo_package_dir |> Array.to_list in
  List.filter
    (fun file -> not @@ Sys.is_directory (ligo_package_dir // file))
    files
  |> List.iter (fun file -> Sys.remove (ligo_package_dir // file))
  |> Lwt.return

(* List.map (fun pkg ->
   let* json = get_json @@ fst pkg in
   download_to_a_file @@ get_tarball_url pkg json) pkgs |>
   |> Lwt.return *)

let _print_list_list_lwt monster =
  let* lst_lst = monster in
  List.iter
    (fun lst ->
      List.iter (fun (pkg, ver) -> Printf.printf "%s -> %s" pkg ver) lst;
      print_newline ())
    lst_lst
  |> Lwt.return

(* create index.json *)
(* {
     "root" : "<name-from-package.json>@link-dev:./package.json"
     "node" : {
       "<name-of-dependency-in-package.json>@<version>@<some-hash>" : {
         "id" : "<name-of-dependency-in-package.json>@<version>@<some-hash>",
         "name": "<name-of-dependency-in-package.json>",
         "version": "<version>",
         "source": {
           "type" : "install",
           "source": [
             "archive:https://packages.ligolang.org/-/api/<dependency-name>/-/<dependency-name>-<version>.tgz#sha1:<shasum-in-api-json>"
           ]
         },
         "overrides": [],
         "dependencies":["<array-of-deps-in-api-json>"],
         "devDependencies":["<array-of-dev-deps-in-api-json>"]
       },
       ...
       ...
       do the same for all deps
     },
     "<name-from-package.json>@link-dev:./package.json": {
       "id" : "<name-from-package.json>@link-dev:./package.json",
       "name" : "<name-from-package.json>",
       "version": "link-dev:./package.json",
       "source": {
         "type": "link-dev",
         "path": ".",
         "manifest": "package.json"
       },
       "overrides":[],
       "dependencies":["<dependency-in-api-json>@<version>@<some-hash>"],
       "devDependencies": []
     }
   } *)
let get_package_tree package_json =
  let dependencies = Json.Util.member "dependencies" package_json in
  let name = Json.Util.member "name" package_json |> Json.to_string |> trim in
  let version =
    Json.Util.member "version" package_json |> Json.to_string |> trim
  in
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

let _ =
  (* Printf.printf "%s\n" @@ Json.to_string package_json; *)
  Lwt_main.run @@ get_package_tree package_json
