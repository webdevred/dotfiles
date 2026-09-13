#!/usr/bin/env -S ocaml -I +unix -I +str unix.cma str.cma

open Printf
open Unix

let buffer_size = 8192

let buffer = Bytes.create buffer_size

let file_copy overwrite input_name output_name =
  if (not overwrite) && Sys.file_exists output_name then ()
  else begin
    let fd_in = openfile input_name [O_RDONLY] 0 in
    let fd_out = openfile output_name [O_WRONLY; O_CREAT; O_TRUNC] 0o666 in
    let rec copy_loop () =
      match read fd_in buffer 0 buffer_size with
      | 0 -> ()
      | r ->
          let rec write_all off left =
            if left > 0 then
              let w = write fd_out buffer off left in
              write_all (off + w) (left - w)
          in
          write_all 0 r ; copy_loop ()
    in
    copy_loop () ; close fd_in ; close fd_out
  end

let backup_regexp = Str.regexp {|.*\..*backup|}

let user_regexp = Str.regexp {|^..-user-.*|}

let rec mkdir_p dir =
  if Sys.file_exists dir then begin
    if not (Sys.is_directory dir) then
      failwith
        (sprintf
           "%s already exists but is not a directory, remove it and retry"
           dir )
  end
  else begin
    mkdir_p (Filename.dirname dir) ;
    try mkdir dir 0o755 with Unix_error (EEXIST, _, _) -> ()
  end

let rec copy_tree action overwrite only_user src dst =
  let entries = Sys.readdir src in
  Array.iter
    (fun name ->
      let src_path = Filename.concat src name in
      let dst_path = Filename.concat dst name in
      match (lstat src_path).st_kind with
      | S_DIR -> copy_tree action overwrite only_user src_path dst_path
      | S_REG ->
          let is_backup = Str.string_match backup_regexp name 0 in
          let is_user = Str.string_match user_regexp name 0 in
          if (not is_backup) && ((not only_user) || is_user) then begin
            mkdir_p dst ;
            printf "%s %s\n" action src_path ;
            file_copy overwrite src_path dst_path
          end
      | _ -> () )
    entries

let rec chown_r dir uid gid =
  match (Unix.lstat dir).st_kind with
  | S_REG -> Unix.chown dir uid gid
  | S_DIR ->
      Unix.chown dir uid gid ;
      Sys.readdir dir
      |> Array.iter (fun name -> chown_r (Filename.concat dir name) uid gid)
  | _ -> ()

let usage_and_exit prog =
  eprintf
    "usage: %s i|f[o]\n\
    \  i  = install repo -> /etc/portage\n\
    \  f  = fetch /etc/portage -> repo\n\
    \  o  = allow overwriting existing files\n"
    prog ;
  exit 1

let () =
  let args = Sys.argv in
  let cwd = Sys.getcwd () in
  if Array.length args <> 2 then usage_and_exit args.(0) ;
  let flags = args.(1) in
  let has c = String.contains flags c in
  let only_known =
    String.for_all (fun c -> c = 'i' || c = 'f' || c = 'o') flags
  in
  let has_i = has 'i' and has_f = has 'f' in
  if (not only_known) || (has_i && has_f) || ((not has_i) && not has_f) then
    usage_and_exit args.(0) ;
  let overwrite = has 'o' in
  if has_i then
    copy_tree "copying" overwrite false
      (Filename.concat cwd "gentoo")
      "/etc/portage"
  else begin
    let dest = Filename.concat cwd "gentoo" in
    let self_stat = Unix.lstat (cwd ^ "/gentoo-install.ml") in
    copy_tree "fetching" overwrite true "/etc/portage" dest ;
    chown_r dest self_stat.st_uid self_stat.st_gid
  end
