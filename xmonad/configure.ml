#!/usr/bin/env -S ocaml -I +unix -I +str unix.cma str.cma

open Unix

let polybar_config = Unix.getenv "HOME" ^ "/.config/polybar/config.ini"
let polybar_choice = Unix.getenv "HOME" ^ "/.config/xmonad/polybar.txt"

(* parse [bar/name] sections from polybar config *)
let parse_bars filename =
  let bar_re = Str.regexp "^\\[bar/\\([^]]+\\)\\]" in
  In_channel.with_open_bin filename In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
         if Str.string_match bar_re line 0 then
           Some (Str.matched_group 1 line)
         else None)

(* read current selection if it exists *)
let read_current () =
  try
    let s = In_channel.with_open_bin polybar_choice In_channel.input_all in
    let s = String.trim s in
    if String.length s > 0 then String.split_on_char ' ' s else []
  with Sys_error _ -> []

(* write chosen bars to polybar.txt *)
let write_choice bars =
  Out_channel.with_open_bin polybar_choice (fun oc ->
      Out_channel.output_string oc (String.concat " " bars ^ "\n"))

(* print bars with indexes, marking currently selected ones *)
let print_bars bars selected =
  Printf.printf "Available polybars:\n" ;
  List.iteri
    (fun i bar ->
      let marker = if List.mem bar selected then " *" else "" in
      Printf.printf "  %d: %s%s\n" i bar marker)
    bars

let print_selected selected =
  if selected = [] then Printf.printf "No bars currently selected.\n"
  else Printf.printf "Current selection: %s\n" (String.concat ", " selected)

(* parse a comma-separated list of indexes *)
let parse_indexes input nbars =
  let tokens = String.split_on_char ',' (String.trim input) in
  let results =
    List.filter_map
      (fun token ->
        let token = String.trim token in
        match int_of_string_opt token with
        | Some n when n >= 0 && n < nbars -> Some (Ok n)
        | Some n ->
            Some
              (Error
                 (Printf.sprintf "index out of bounds: %d (0-%d)" n (nbars - 1)))
        | None -> Some (Error (Printf.sprintf "invalid input: %s" token)))
      tokens
  in
  let errors =
    List.filter_map (function Error e -> Some e | Ok _ -> None) results
  in
  let indexes =
    List.filter_map (function Ok n -> Some n | Error _ -> None) results
  in
  if errors <> [] then Error (String.concat ", " errors)
  else if indexes = [] then Error "please enter at least one index"
  else Ok indexes

type action = Continue | Delete | Select of int list

let parse_action input nbars =
  let input = String.trim input in
  if input = "" || input = "c" || input = "continue" then Ok Continue
  else if input = "d" || input = "delete" then Ok Delete
  else
    match parse_indexes input nbars with
    | Ok indexes -> Ok (Select indexes)
    | Error e -> Error e

(* the interactive selection loop — selecting already-selected bars toggles them off *)
let rec configure_loop bars selected =
  print_selected selected ;
  let nbars = List.length bars in
  Printf.printf "Select bars by index, d(elete) or c(ontinue) (0-%d, c, d): %!"
    (nbars - 1) ;
  let input = input_line Stdlib.stdin in
  match parse_action input nbars with
  | Error e ->
      Printf.printf "Error: %s\n" e ;
      configure_loop bars selected
  | Ok Continue -> selected
  | Ok Delete ->
      Printf.printf "Selection cleared.\n" ;
      configure_loop bars []
  | Ok (Select indexes) ->
      let newly_selected = List.map (List.nth bars) indexes in
      (* if all are already selected, toggle them off; otherwise add missing ones *)
      let all_present =
        List.for_all (fun b -> List.mem b selected) newly_selected
      in
      let updated =
        if all_present then
          List.filter (fun b -> not (List.mem b newly_selected)) selected
        else
          selected
          @ List.filter (fun b -> not (List.mem b selected)) newly_selected
      in
      configure_loop bars updated

let () =
  let bars = parse_bars polybar_config in
  if bars = [] then (
    Printf.printf "No bars found in %s\n" polybar_config ;
    exit 1 ) ;
  let current = read_current () in
  print_bars bars current ;
  let selected = configure_loop bars current in
  if selected = [] then
    Printf.printf "No bars selected, polybar.txt not updated.\n"
  else (
    write_choice selected ;
    Printf.printf "Saved: %s\n" (String.concat " " selected) )
