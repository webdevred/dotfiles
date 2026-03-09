open Unix

(* derive src and dst from script location + dest_places.txt *)
let script_dir = Filename.dirname Sys.argv.(0)
let dotfiles_root = Filename.dirname script_dir
let dir_name = Filename.basename script_dir

let dst =
  let home = Unix.getenv "HOME" in
  let lines =
    In_channel.with_open_bin
      (dotfiles_root ^ "/dest_places.txt")
      In_channel.input_all
    |> String.split_on_char '\n'
  in
  List.find_map
    (fun line ->
      if String.starts_with line ~prefix:"#" || String.trim line = "" then None
      else
        match String.split_on_char ':' line with
        | src :: dest :: _ when String.equal src dir_name ->
            Some (home ^ "/" ^ dest)
        | _ -> None )
    lines
  |> Option.get

let extra_src = script_dir ^ "/extra"
let extra_enabled_dst = dst ^ "/extra-enabled"
let opt_dst = dst ^ "/opt"

let run cmd =
  Printf.printf "$ %s\n%!" cmd ;
  ignore (Unix.system cmd)

let install_file file =
  if String.equal file "haskell-mode-git.el" then (
    if not (Sys.file_exists opt_dst) then Unix.mkdir opt_dst 0o700 ;
    let haskell_mode_dir = opt_dst ^ "/haskell-mode" in
    if Sys.file_exists haskell_mode_dir then
      run (Printf.sprintf "cd %s && git pull && make" haskell_mode_dir)
    else
      run
        (Printf.sprintf
           "cd %s && git clone https://github.com/haskell/haskell-mode.git && \
            cd haskell-mode && make"
           opt_dst) ) ;
  if not (Sys.file_exists extra_enabled_dst) then
    Unix.mkdir extra_enabled_dst 0o700 ;
  let link_src = extra_src ^ "/" ^ file in
  let link_dst = extra_enabled_dst ^ "/" ^ file in
  if not (Sys.file_exists link_dst) then (
    Printf.printf "symlinking %s -> %s\n%!" link_src link_dst ;
    Unix.symlink link_src link_dst )
  else Printf.printf "%s already linked, skipping\n%!" file ;
  Printf.printf "done with %s\n%!" file

let () =
  let files = Sys.readdir extra_src in
  Array.sort String.compare files ;
  Array.iter
    (fun file ->
      if not (String.ends_with file ~suffix:"~") then (
        Printf.printf "Do you want %s? " file ;
        flush Stdlib.stdout ;
        let yn = try input_line Stdlib.stdin with End_of_file -> "" in
        if String.length yn > 0 && (yn.[0] = 'y' || yn.[0] = 'Y') then
          install_file file ) )
    files
