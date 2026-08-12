#!/usr/bin/env -S ocaml -I +unix -I +str unix.cma str.cma

open Unix

(* file we store dest places in *)
let filename = "dest_places.txt"

(* data types *)
type dot_file =
  {source: string; destination: string; mode: int option; symlink: bool}

type action = Install | Uninstall | Configure

let source_top_dir source =
  match String.split_on_char '/' source with dir :: _ -> dir | [] -> source

(* parse command line input *)
let get_chosen_dotfiles args =
  let len = Array.length args in
  if len > 2 then Some (Array.to_list (Array.sub args 2 (len - 2))) else None

let get_chosen_action args =
  let len = Array.length args in
  if len >= 2 then
    match Array.get args 1 with
    | "i" -> Install
    | "u" -> Uninstall
    | "c" -> Configure
    | a -> raise (Failure a)
  else Install

(* Determine whether the dotfile should be symlinked or hardlinked. If the
   value is "hardlink", we use hardlinks (or mirrored directories with
   hardlinked files). For any other value, we default to symlinking. *)
let should_symlink opt_syml =
  not (String.equal (Option.value opt_syml ~default:"symlink") "hardlink")

let parse_mode maybe_mode =
  match maybe_mode with
  | Some mode -> (
    try Some (int_of_string ("0o" ^ mode)) with Failure _ -> None )
  | None -> None

(* parse the lines in dest file *)
let line_to_dot_file s =
  let home = Unix.getenv "HOME" in
  match String.split_on_char ':' s with
  | source :: dest :: rest ->
      let destination = home ^ "/" ^ dest in
      let symlink = should_symlink (List.nth_opt rest 0) in
      let mode = parse_mode (List.nth_opt rest 1) in
      {source; destination; mode; symlink}
  | _ -> failwith (Printf.sprintf "malformed line in dest_places.txt: %s" s)

(* skip comments lines *)
let shouldnt_skip_line str =
  not
    (String.starts_with ~prefix:"#" str || String.equal (String.trim str) "")

(* parse the dest file *)
let places =
  In_channel.with_open_bin filename In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter shouldnt_skip_line
  |> List.map line_to_dot_file

(* Match the whole source or its top directory, never a substring: "haskell"
   selects every haskell/* entry, but a stray "a" selects nothing. *)
let dotfile_is_chosen chosen place =
  let matches name =
    String.equal place.source name
    || String.equal (source_top_dir place.source) name
  in
  List.exists matches chosen

(* logic for doing installation of dotfiles *)
(* Destinations nest deeper than their parents exist on a fresh machine,
   .local/bin being the usual one. *)
let rec create_dir dir =
  if not (Sys.file_exists dir) then (
    let parent = Filename.dirname dir in
    if not (String.equal parent dir) then create_dir parent ;
    Sys.mkdir dir 0o700 )

let set_permissions source current_mode maybe_mode =
  match maybe_mode with
  | Some mode ->
      if not (current_mode == mode) then (
        Printf.printf "setting permissions to 0%o for %s\n" mode source ;
        chmod source mode )
  | None -> ()

let rec link_dotfile please_symlink maybe_mode source destination =
  let file_stat = stat source in
  if file_stat.st_kind == S_DIR && please_symlink then (
    (* symlink the whole directory in one shot *)
    if not (Sys.file_exists destination) then (
      let dest_dir = Filename.dirname destination in
      if not (Sys.file_exists dest_dir) then create_dir dest_dir ;
      Printf.printf "symlinking dir %s %s\n" source destination ;
      symlink source destination ) )
  else if file_stat.st_kind == S_DIR then
    (* hardlink mode: recurse and hardlink individual files *)
    let files = Sys.readdir source in
    for i = 0 to Array.length files - 1 do
      let single_file = files.(i) in
      (* Editor droppings and vendored checkouts are skipped, but ordinary
         dotfiles such as .dir-locals.el are the point of the repo. *)
      if
        not
          ( String.starts_with single_file ~prefix:"#"
          || String.starts_with single_file ~prefix:".#"
          || String.ends_with single_file ~suffix:"~"
          || String.equal single_file ".git" )
      then
        link_dotfile please_symlink maybe_mode
          (source ^ "/" ^ single_file)
          (destination ^ "/" ^ single_file)
    done
  else (
    set_permissions source file_stat.st_perm maybe_mode ;
    if not (Sys.file_exists destination) then (
      let dest_dir = Filename.dirname destination in
      if not (Sys.file_exists dest_dir) then create_dir dest_dir ;
      if please_symlink then (
        Printf.printf "symlinking %s %s\n" source destination ;
        symlink source destination )
      else (
        Printf.printf "hardlinking %s %s\n" source destination ;
        link source destination ) ) )

let install cwd places =
  let dotfile_basepath = cwd ^ "/" in
  let do_install place =
    let source = dotfile_basepath ^ place.source in
    let destination = place.destination in
    let please_symlink = place.symlink in
    let maybe_mode = place.mode in
    link_dotfile please_symlink maybe_mode source destination
  in
  List.iter do_install places

(* logic for uninstallation of dotfiles *)
let confirm question =
  Printf.printf "%s [y/N] %!" question ;
  try
    match String.lowercase_ascii (String.trim (input_line Stdlib.stdin)) with
    | "y" | "yes" -> true
    | _ -> false
  with End_of_file -> false

(* A symlink is ours alone, so it goes without asking. A real directory is
   shared with whatever the program writes there itself, such as elpa/ and
   eln-cache/ under .config/emacs, so that one needs a yes. *)
let uninstall places =
  let rec rmrf path =
    match (Unix.lstat path).st_kind with
    | Unix.S_DIR ->
        Sys.readdir path
        |> Array.iter (fun name -> rmrf (Filename.concat path name)) ;
        Unix.rmdir path
    | _ -> Sys.remove path
  in
  let remove_dotfile path =
    try
      match (Unix.lstat path).st_kind with
      | Unix.S_DIR ->
          if
            confirm
              (Printf.sprintf "delete %s and everything inside it?" path)
          then (
            Printf.printf "uninstalling %s\n" path ;
            rmrf path )
          else Printf.printf "skipping %s\n" path
      | _ ->
          Printf.printf "uninstalling %s\n" path ;
          Unix.unlink path
    with Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  in
  List.iter (fun dotfile -> remove_dotfile dotfile.destination) places

(* entry *)
let perform_action action maybe_chosen_sources cwd places =
  let chosen_places =
    match maybe_chosen_sources with
    | Some chosen -> List.filter (dotfile_is_chosen chosen) places
    | None -> places
  in
  match action with
  | Install -> install cwd chosen_places
  | Uninstall -> uninstall chosen_places
  | Configure ->
      let seen = Hashtbl.create 8 in
      List.iter
        (fun place ->
          let dir = source_top_dir place.source in
          if not (Hashtbl.mem seen dir) then (
            Hashtbl.add seen dir true ;
            let configure_ml = cwd ^ "/" ^ dir ^ "/configure.ml" in
            if Sys.file_exists configure_ml then (
              let cmd =
                Printf.sprintf
                  "ocaml -I +unix -I +str unix.cma str.cma %s"
                  configure_ml
              in
              Printf.printf "configuring %s\n%!" dir ;
              ignore (Unix.system cmd) ) ) )
        chosen_places

let () =
  let cwd = Sys.getcwd () in
  let all_places = places in
  let chosen_dotfiles = get_chosen_dotfiles Sys.argv in
  let action = get_chosen_action Sys.argv in
  perform_action action chosen_dotfiles cwd all_places
