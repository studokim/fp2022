(** Copyright 2022-2023, studokim and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Reads the whole file. Doesn't handle errors.*)
let read_file filepath =
  let channel = open_in filepath in
  let program = really_input_string channel (in_channel_length channel) in
  close_in channel;
  program
;;

(** Simply wraps [String.concat] with empty separator. *)
let concat = String.concat ""
