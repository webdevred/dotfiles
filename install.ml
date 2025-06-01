#!/usr/bin/env -S ocaml -I +unix unix.cma

open Unix

(* file we store dest places in *)
let filename = "dest_places.txt"

(* data types *)
type dot_file =
  {source: string; destination: string; mode: int option; symlink: bool}

type action = Install | Uninstall | Configure

(* parse command line input *)
let get_chosen_dotfiles args =
  let len = Array.length args in
  if len > 2 then Some (Array.to_list (Array.sub args 2 (len - 2))) else None

let get_choosen_action args =
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
  String.equal (Option.value opt_syml ~default:"symlink") "symlink"

let parse_mode maybe_mode =
  match maybe_mode with
  | Some mode -> (
    try Some (int_of_string ("0o" ^ mode)) with Failure _ -> None )
  | None -> None

(* parse the lines in dest file *)
let line_to_dot_file s =
  let home = Unix.getenv "HOME" in
  let list = String.split_on_char ':' s in
  let source = List.nth list 0 in
  let destination = home ^ "/" ^ List.nth list 1 in
  let symlink = should_symlink (List.nth_opt list 2) in
  let mode = parse_mode (List.nth_opt list 3) in
  {source; destination; mode; symlink}

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

let dotfile_is_chosen chosen place =
  List.exists (fun x -> String.compare x place.source == 0) chosen

(* logic for doing installation of dotfiles *)
let create_dir dir = if not (Sys.file_exists dir) then Sys.mkdir dir 0o700

let set_permissions source current_mode maybe_mode =
  match maybe_mode with
  | Some mode ->
      if not (current_mode == mode) then (
        Printf.printf "setting permissions to 0%o for %s\n" mode source ;
        chmod source mode )
  | None -> ()

let rec link_dotfile please_symlink maybe_mode source destination =
  let file_stat = stat source in
  if file_stat.st_kind == S_DIR then
    let files = Sys.readdir source in
    for i = 0 to Array.length files - 1 do
      let single_file = files.(i) in
      if
        not
          ( String.starts_with single_file ~prefix:"."
          || String.starts_with single_file ~prefix:"#"
          || String.ends_with single_file ~suffix:"~" )
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

(* entry *)
let perform_action action maybe_chosen_sources cwd places =
  let chosen_places =
    match maybe_chosen_sources with
    | Some chosen -> List.filter (dotfile_is_chosen chosen) places
    | None -> places
  in
  match action with
  | Install -> install cwd chosen_places
  | Uninstall -> () (* uninstall chosen_dotfiles *)
  | Configure -> () (* confugure chosen_dotfiles *)

let () =
  let cwd = Sys.getcwd () in
  let all_places = places in
  let chosen_dotfiles = get_chosen_dotfiles Sys.argv in
  let action = get_choosen_action Sys.argv in
  perform_action action chosen_dotfiles cwd all_places
