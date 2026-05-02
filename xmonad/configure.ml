#!/usr/bin/env -S ocaml -I +unix -I +str unix.cma str.cma

open Unix

let home = Unix.getenv "HOME"
let polybar_config = home ^ "/.config/polybar/config.ini"
let polybar_choice = home ^ "/.config/xmonad/polybar.txt"
let pool_choice    = home ^ "/.config/xmonad/pool.txt"

(* open /dev/tty directly for input, bypassing whatever stdin is *)
let tty = open_in "/dev/tty"

(* parse [bar/name] sections from polybar config *)
let parse_bars filename =
  let bar_re = Str.regexp "^\\[bar/\\([^]]+\\)\\]" in
  In_channel.with_open_bin filename In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
         if Str.string_match bar_re line 0 then
           Some (Str.matched_group 1 line)
         else None)

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
  (match Unix.close_process_in ic with
   | WEXITED 0 -> ()
   | _ -> prerr_endline "warning: zpool list failed") ;
  pools

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
      Out_channel.output_string oc (String.concat " " items ^ "\n"))

(* print items with indexes, marking currently selected ones *)
let print_items label items selected =
  Printf.printf "Available %s:\n" label ;
  List.iteri
    (fun i item ->
      let marker = if List.mem item selected then " *" else "" in
      Printf.printf "  %d: %s%s\n" i item marker)
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
                 (Printf.sprintf "index out of bounds: %d (0-%d)" i (n - 1)))
        | None -> Some (Error (Printf.sprintf "invalid input: %s" token)))
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

(* the interactive selection loop — selecting already-selected items toggles them off *)
let rec configure_loop label items selected =
  print_selected label selected ;
  let n = List.length items in
  Printf.printf "Select %s by index, d(elete) or c(ontinue) (0-%d, c, d): %!"
    label (n - 1) ;
  let input = try input_line tty with End_of_file -> print_newline () ; "c" in
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
      let all_present =
        List.for_all (fun b -> List.mem b selected) newly
      in
      let updated =
        if all_present then
          List.filter (fun b -> not (List.mem b newly)) selected
        else
          selected
          @ List.filter (fun b -> not (List.mem b selected)) newly
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
    ( if chosen = [] then
        Printf.printf "No %s selected, %s not updated.\n" label choice_path
      else (
        write_choice choice_path chosen ;
        Printf.printf "Saved %s: %s\n" label (String.concat " " chosen) ) ) ;
    chosen

let () =
  let bars = parse_bars polybar_config in
  if bars = [] then (
    Printf.printf "No bars found in %s\n" polybar_config ;
    exit 1 ) ;
  let _ = select_and_save "polybars" bars polybar_choice in
  let pools = list_pools () in
  let _ = select_and_save "pools" pools pool_choice in
  close_in tty
