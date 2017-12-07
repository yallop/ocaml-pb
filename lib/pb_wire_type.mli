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

val of_int : int -> t

val to_int : t -> int

val to_string : t -> string

val pp : Format.formatter -> t -> unit
