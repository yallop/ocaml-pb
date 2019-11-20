(*
 * Copyright (c) 2017 Jeremy Yallop <yallop@docker.com>.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type t =
  | Varint
  | Sixty_four
  | Length_delimited
  | Start_group
  | End_group
  | Thirty_two

val of_int : int -> t

val to_int : t -> int

val to_string : t -> string

val compare : t -> t -> int

val pp : Format.formatter -> t -> unit
