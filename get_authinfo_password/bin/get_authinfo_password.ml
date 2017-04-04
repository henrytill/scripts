#! /usr/bin/env ocaml
#use "topfind"
#require "str"
#require "unix"

let close channel =
  ignore (Unix.close_process_in channel)

let rec search channel regexp =
  try
    let line = input_line channel in
    if Str.string_match regexp line 0 then
      begin
        close channel;
        Some (Str.matched_group 1 line)
      end
    else
      search channel regexp
  with
  | End_of_file ->
      close channel;
      None
  | exn ->
      close channel;
      raise exn

let get_authinfo_password authinfo machine login port =
  let s = "machine " ^ machine ^ " login " ^ login ^ " password \\([^ ]*\\) port " ^ port in
  let r = Str.regexp s in
  let c = Unix.open_process_in ("gpg2 -q --no-tty -d " ^ authinfo) in
  search c r

let () =
  let open Sys in
  if Array.length argv = 5 then
    match get_authinfo_password argv.(1) argv.(2) argv.(3) argv.(4) with
    | Some v ->
        print_endline v;
        exit 0
    | None ->
        exit 0
  else
    prerr_endline "Usage: get_authinfo_password.ml <file> <machine> <login> <port>";
    exit 2
