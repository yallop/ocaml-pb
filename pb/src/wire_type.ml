(*
 * Copyright (c) 2017 Jeremy Yallop <yallop@docker.com>.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type t =
  | Varint
  (** (0) Used for int32, int64, uint32, uint64,
      sint32, sint64, bool, enum *)
  | Sixty_four
  (** (1) Used for fixed64, sfixed64, double *)
  | Length_delimited
  (** (2) Used for string, bytes, embedded messages,
      packed repeated fields *)
  | Start_group
  (** (3) Used for groups (deprecated) *)
  | End_group
  (** (4) Used for groups (deprecated) *)
  | Thirty_two
  (** (5) Used for fixed32, sfixed32, float *)

let of_int = function
  | 0 -> Varint
  | 1 -> Sixty_four
  | 2 -> Length_delimited
  | 3 -> Start_group
  | 4 -> End_group
  | 5 -> Thirty_two
  | n -> Printf.kprintf invalid_arg "Wire_type.of_int (%d)" n

let to_int = function
  | Varint           -> 0
  | Sixty_four       -> 1
  | Length_delimited -> 2
  | Start_group      -> 3
  | End_group        -> 4
  | Thirty_two       -> 5

let to_string = function
  | Varint           -> "Varint"
  | Sixty_four       -> "Sixty_four"
  | Length_delimited -> "Length_delimited"
  | Start_group      -> "Start_group"
  | End_group        -> "End_group"
  | Thirty_two       -> "Thirty_two"

let pp fmt v = Format.pp_print_string fmt (to_string v)

let compare = compare
