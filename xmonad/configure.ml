#!/usr/bin/env -S ocaml -I +unix -I +str unix.cma str.cma

open Unix

let home = Unix.getenv "HOME"

let polybar_config = home ^ "/.config/polybar/config.ini"

let polybar_choice = home ^ "/.config/xmonad/polybar.txt"

let pool_choice = home ^ "/.config/xmonad/pool.txt"

let polybar_env = home ^ "/.config/xmonad/polybar.env"

let default_mount1 = "/mnt/HDD"

let default_modules_right =
  "zfs filesystem pulseaudio xkeyboard memory cpu wlan eth date"

let modules_right_no_wifi =
  "zfs filesystem pulseaudio xkeyboard memory cpu eth date"

let default_monitor_laptop = "eDP-1"

(* open /dev/tty directly for input, bypassing whatever stdin is *)
let tty = open_in "/dev/tty"

(* split the whole ini into (section name, lines) chunks, e.g. ("bar/laptop",
   [...]), ("module/filesystem", [...]) *)
let parse_sections config_text =
  let header_re = Str.regexp "^\\[\\([^]]+\\)\\]" in
  let close_section sections name lines =
    match name with
    | Some n -> (n, List.rev lines) :: sections
    | None -> sections
  in
  let rec loop sections name lines = function
    | [] -> List.rev (close_section sections name lines)
    | line :: rest ->
        if Str.string_match header_re line 0 then
          let new_name = Str.matched_group 1 line in
          loop (close_section sections name lines) (Some new_name) [] rest
        else loop sections name (line :: lines) rest
  in
  loop [] None [] (String.split_on_char '\n' config_text)

let parse_bars sections =
  List.filter_map
    (fun (name, _) ->
      if String.starts_with name ~prefix:"bar/" then
        Some (String.sub name 4 (String.length name - 4))
      else None )
    sections

(* value of "key = value" in a section's lines, ignoring surrounding space *)
let get_field lines key =
  List.find_map
    (fun line ->
      match String.index_opt line '=' with
      | Some i when String.trim (String.sub line 0 i) = key ->
          Some
            (String.trim
               (String.sub line (i + 1) (String.length line - i - 1)) )
      | _ -> None )
    lines

(* env var names referenced as ${env:NAME...} anywhere in the given lines *)
let env_vars_in_lines lines =
  let re = Str.regexp "\\${env:\\([A-Za-z_][A-Za-z0-9_]*\\)" in
  List.concat_map
    (fun line ->
      let rec loop pos acc =
        match Str.search_forward re line pos with
        | i ->
            let var = Str.matched_group 1 line in
            loop (i + String.length var) (var :: acc)
        | exception Not_found -> acc
      in
      loop 0 [] )
    lines

(* module names a bar loads, from its modules-left/modules-right fields *)
let bar_modules sections bar =
  match List.assoc_opt ("bar/" ^ bar) sections with
  | None -> []
  | Some lines ->
      let names_of key =
        match get_field lines key with
        | Some v ->
            String.split_on_char ' ' v |> List.filter (fun s -> s <> "")
        | None -> []
      in
      names_of "modules-left" @ names_of "modules-right"

(* env vars a bar's own fields (e.g. monitor) and the modules it loads
   reference — so config only asks about what the selected bars use *)
let env_vars_for_bar sections bar =
  let bar_lines =
    Option.value (List.assoc_opt ("bar/" ^ bar) sections) ~default:[]
  in
  let module_vars =
    List.concat_map
      (fun m ->
        match List.assoc_opt ("module/" ^ m) sections with
        | Some lines -> env_vars_in_lines lines
        | None -> [] )
      (bar_modules sections bar)
  in
  env_vars_in_lines bar_lines @ module_vars

let env_vars_for_bars sections bars =
  List.concat_map (env_vars_for_bar sections) bars |> List.sort_uniq compare

(* drain all lines from a channel until EOF *)
let read_lines ic =
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  loop []

(* list zpools — one pool name per line, no header, no padding *)
let list_pools () =
  let ic = Unix.open_process_in "zpool list -H -o name" in
  let pools = read_lines ic in
  ( match Unix.close_process_in ic with
  | WEXITED 0 -> ()
  | _ -> prerr_endline "warning: zpool list failed" ) ;
  pools

(* list connected monitors — last field of each "xrandr --listmonitors" line
   after the header, e.g. " 0: +*eDP-1 1920/276x1080/155+0+0 eDP-1" *)
let list_monitors () =
  let ic = Unix.open_process_in "xrandr --listmonitors" in
  let lines = read_lines ic in
  ( match Unix.close_process_in ic with
  | WEXITED 0 -> ()
  | _ -> prerr_endline "warning: xrandr --listmonitors failed" ) ;
  let parse_line line =
    match
      List.filter
        (fun s -> s <> "")
        (String.split_on_char ' ' (String.trim line))
    with
    | [] -> None
    | fields -> Some (List.nth fields (List.length fields - 1))
  in
  match lines with
  | _header :: rest -> List.filter_map parse_line rest
  | [] -> []

(* read current selection if it exists *)
let read_current path =
  try
    let s = In_channel.with_open_bin path In_channel.input_all in
    let s = String.trim s in
    if String.length s > 0 then String.split_on_char ' ' s else []
  with Sys_error _ -> []

(* write chosen items to a path *)
let write_choice path items =
  Out_channel.with_open_bin path (fun oc ->
      Out_channel.output_string oc (String.concat " " items ^ "\n") )

(* print items with indexes, marking currently selected ones *)
let print_items label items selected =
  Printf.printf "Available %s:\n" label ;
  List.iteri
    (fun i item ->
      let marker = if List.mem item selected then " *" else "" in
      Printf.printf "  %d: %s%s\n" i item marker )
    items

let print_selected label selected =
  if selected = [] then Printf.printf "No %s currently selected.\n" label
  else Printf.printf "Current %s: %s\n" label (String.concat ", " selected)

(* parse a comma-separated list of indexes *)
let parse_indexes input n =
  let tokens = String.split_on_char ',' (String.trim input) in
  let results =
    List.filter_map
      (fun token ->
        let token = String.trim token in
        match int_of_string_opt token with
        | Some i when i >= 0 && i < n -> Some (Ok i)
        | Some i ->
            Some
              (Error
                 (Printf.sprintf "index out of bounds: %d (0-%d)" i (n - 1))
              )
        | None -> Some (Error (Printf.sprintf "invalid input: %s" token)) )
      tokens
  in
  let errors =
    List.filter_map (function Error e -> Some e | Ok _ -> None) results
  in
  let indexes =
    List.filter_map (function Ok i -> Some i | Error _ -> None) results
  in
  if errors <> [] then Error (String.concat ", " errors)
  else if indexes = [] then Error "please enter at least one index"
  else Ok indexes

type action = Continue | Delete | Select of int list

let parse_action input n =
  let input = String.trim input in
  if input = "" || input = "c" || input = "continue" then Ok Continue
  else if input = "d" || input = "delete" then Ok Delete
  else
    match parse_indexes input n with
    | Ok indexes -> Ok (Select indexes)
    | Error e -> Error e

(* the interactive selection loop — selecting already-selected items toggles
   them off *)
let rec configure_loop label items selected =
  print_selected label selected ;
  let n = List.length items in
  Printf.printf "Select %s by index, d(elete) or c(ontinue) (0-%d, c, d): %!"
    label (n - 1) ;
  let input =
    try input_line tty with End_of_file -> print_newline () ; "c"
  in
  match parse_action input n with
  | Error e ->
      Printf.printf "Error: %s\n" e ;
      configure_loop label items selected
  | Ok Continue -> selected
  | Ok Delete ->
      Printf.printf "Selection cleared.\n" ;
      configure_loop label items []
  | Ok (Select indexes) ->
      let newly = List.map (List.nth items) indexes in
      let all_present = List.for_all (fun b -> List.mem b selected) newly in
      let updated =
        if all_present then
          List.filter (fun b -> not (List.mem b newly)) selected
        else
          selected @ List.filter (fun b -> not (List.mem b selected)) newly
      in
      configure_loop label items updated

(* run one selection round: list items, prompt, save *)
let select_and_save label items choice_path =
  if items = [] then (
    Printf.printf "No %s available.\n" label ;
    [] )
  else
    let current = read_current choice_path in
    print_items label items current ;
    let chosen = configure_loop label items current in
    if chosen = [] then
      Printf.printf "No %s selected, %s not updated.\n" label choice_path
    else (
      write_choice choice_path chosen ;
      Printf.printf "Saved %s: %s\n" label (String.concat " " chosen) ) ;
    chosen

(* double-quote a value for /bin/sh, escaping what's special inside double
   quotes, so a value containing a space (a mountpoint like "/media/debbie/My
   Passport") round-trips correctly through sourcing polybar.env *)
let shell_quote value =
  let buf = Buffer.create (String.length value + 2) in
  Buffer.add_char buf '"' ;
  String.iter
    (fun c ->
      ( match c with
      | '"' | '\\' | '$' | '`' -> Buffer.add_char buf '\\'
      | _ -> () ) ;
      Buffer.add_char buf c )
    value ;
  Buffer.add_char buf '"' ;
  Buffer.contents buf

let shell_unquote value =
  let n = String.length value in
  if n >= 2 && value.[0] = '"' && value.[n - 1] = '"' then (
    let buf = Buffer.create n in
    let i = ref 1 in
    while !i < n - 1 do
      if value.[!i] = '\\' && !i + 1 < n - 1 then (
        Buffer.add_char buf value.[!i + 1] ;
        i := !i + 2 )
      else (
        Buffer.add_char buf value.[!i] ;
        incr i )
    done ;
    Buffer.contents buf )
  else value

(* read vars saved by a previous run, e.g. "export
   POLYBAR_MOUNT1=/mnt/HDD" *)
let read_env_file path =
  try
    In_channel.with_open_bin path In_channel.input_all
    |> String.split_on_char '\n'
    |> List.filter_map (fun line ->
        let prefix = "export " in
        if String.starts_with line ~prefix then
          match String.index_opt line '=' with
          | Some i ->
              let key =
                String.sub line (String.length prefix)
                  (i - String.length prefix)
              in
              let value =
                String.sub line (i + 1) (String.length line - i - 1)
              in
              Some (key, shell_unquote value)
          | None -> None
        else None )
  with Sys_error _ -> []

let set_var vars key value =
  if List.mem_assoc key vars then
    List.map (fun (k, v) -> if k = key then (k, value) else (k, v)) vars
  else vars @ [(key, value)]

let write_env_file path vars =
  Out_channel.with_open_bin path (fun oc ->
      List.iter
        (fun (k, v) ->
          Out_channel.output_string oc
            (Printf.sprintf "export %s=%s\n" k (shell_quote v)) )
        vars )

let prompt_default label default =
  Printf.printf "%s [%s] (c to keep, d to clear): %!" label default ;
  let input =
    try String.trim (input_line tty)
    with End_of_file -> print_newline () ; ""
  in
  match String.lowercase_ascii input with
  | "" | "c" | "continue" -> default
  | "d" | "delete" -> ""
  | _ -> input

(* a wireless network interface's directory under /sys/class/net always has a
   "wireless" subdirectory — presence/absence is unambiguous, unlike a
   monitor name, so there's nothing to ask the user *)
let has_wireless_interface () =
  try
    Sys.readdir "/sys/class/net"
    |> Array.exists (fun iface ->
        Sys.file_exists (Printf.sprintf "/sys/class/net/%s/wireless" iface) )
  with Sys_error _ -> false

(* one prompt per plain string var: (env name, prompt label, default) *)
let monitor_var_specs =
  [ ("POLYBAR_MONITOR_LAPTOP", "Laptop bar monitor", default_monitor_laptop)
  ; ("POLYBAR_MOUNT1", "Second mountpoint (empty for none)", default_mount1)
  ]

(* only asks about env vars the currently-selected bars actually reference,
   so a machine running only the laptop bar isn't asked about the desktop's
   big/small-screen monitors *)
let configure_polybar_env relevant_vars =
  if relevant_vars = [] then
    Printf.printf "No polybar bars selected, skipping polybar.env.\n"
  else (
    ( if List.mem "POLYBAR_MONITOR_LAPTOP" relevant_vars then
        match list_monitors () with
        | [] -> ()
        | monitors ->
            Printf.printf "Detected monitors: %s\n"
              (String.concat ", " monitors) ) ;
    let vars = read_env_file polybar_env in
    let vars =
      List.fold_left
        (fun vars (key, label, default) ->
          if List.mem key relevant_vars then
            let current = Option.value (List.assoc_opt key vars) ~default in
            set_var vars key (prompt_default label current)
          else vars )
        vars monitor_var_specs
    in
    let vars =
      if List.mem "POLYBAR_MODULES_RIGHT" relevant_vars then (
        let has_wifi = has_wireless_interface () in
        Printf.printf "Wifi interface %s, %s wlan module.\n"
          (if has_wifi then "detected" else "not detected")
          (if has_wifi then "including" else "excluding") ;
        let modules_right =
          if has_wifi then default_modules_right else modules_right_no_wifi
        in
        set_var vars "POLYBAR_MODULES_RIGHT" modules_right )
      else vars
    in
    write_env_file polybar_env vars ;
    Printf.printf "Saved %s\n" polybar_env )

let () =
  let config_text =
    In_channel.with_open_bin polybar_config In_channel.input_all
  in
  let sections = parse_sections config_text in
  let bars = parse_bars sections in
  if bars = [] then (
    Printf.printf "No bars found in %s\n" polybar_config ;
    exit 1 ) ;
  let chosen_bars = select_and_save "polybars" bars polybar_choice in
  let pools = list_pools () in
  let _ = select_and_save "pools" pools pool_choice in
  configure_polybar_env (env_vars_for_bars sections chosen_bars) ;
  close_in tty
