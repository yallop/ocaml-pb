(*
 * Copyright (c) 2017 Jeremy Yallop <yallop@docker.com>.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module Wire_type : module type of struct include Wire_type end

(** {1} Field types *)

type 'a field_type
(** The type of protobuf field encodings. *)

val bool : bool field_type
(** Varint-encoded booleans *)

val int32 : int32 field_type
(** Varint-encoded signed 32-bit integers.  This is space-inefficient for
    negative numbers; it is recommended to use {!sint32} instead if negative
    numbers are likely to be frequent. *)

val sint32 : int32 field_type
(** Zigzag-encoded signed 32-bit integer.  This is more space-efficient for
    negative numbers than {!int32}. *)

val sfixed32 : int32 field_type
(** Fixed-size encoding of signed 32-bit integers. *)

val int64 : int64 field_type
(** Varint-encoded signed 64-bit integers.  This is space-inefficient for
    negative numbers; it is recommended to use {!sint64} instead if negative
    numbers are likely to be frequent. *)

val sint64 : int64 field_type
(** Zigzag-encoded signed 64-bit integer.  This is more space-efficient for
    negative numbers than {!int64}. *)

val sfixed64 : int64 field_type
(** Fixed-size encoding of signed 64-bit integers. *)

val uint32 : Unsigned.uint32 field_type
(** Varint-encoded unsigned 32-bit integers. *)

val fixed32 : Unsigned.uint32 field_type
(** Fixed-size encoding of unsigned 32-bit integers. *)

val uint64 : Unsigned.uint64 field_type
(** Varint-encoded unsigned 64-bit integers. *)

val fixed64 : Unsigned.uint64 field_type
(** Fixed-size encoding of unsigned 64-bit integers. *)

val double : float field_type
(** Fixed-size encoding of 64-bit floating-point numbers. *)

val float : float field_type
(** Fixed-size encoding of 32-bit floating-point numbers. *)

val string : string field_type
(** Length-delimited encoding of UTF-8 strings. *)

val bytes : Bytes.t field_type
(** Length-delimited encoding of byte strings. *)

type _ msg
(** A value of type [msg] represents a message *) 
  
type _ msgtype
(** A value of type [msgtype] represents a message type *) 

val msg : 'm msgtype -> 'm msg field_type
(** Embedded message field type *)

val read_field : 'a field_type -> 'a Angstrom.t
(** [read_field t] returns an Angstrom parser for a field descsribed by
    [t]. *)

val write_field : 'a field_type -> Faraday.t -> 'a -> unit
(** [write_field t f v] serializes the bytes of the encoding of [v]
    described by [t], in order, to [f]. *)

type _ enum

module type ENUM = sig
  type e
  val t : e enum field_type
  val constant : string -> int32 -> e enum
end

val enum : string -> (module ENUM)
(** Create a new enum type *)

val constant_value : _ enum -> int32
(** The value of a constant *)

type (_,_) field
(** A value of type [(m, t) field] describes a field of type [t] in a
    message of type [m]. *)

module type MESSAGE = sig
  type m
  val t : m msgtype
  val optional : ?default:'a -> 'a field_type -> string -> int ->
    (m, 'a option) field
  val repeated : ?packed:bool -> 'a field_type -> string -> int ->
    (m, 'a list) field
  val required : 'a field_type -> string -> int ->
    (m, 'a) field
end
val message : string -> (module MESSAGE)
(** Create a new message type *)

val pp_field_type : Format.formatter -> 'a field_type -> unit
(** Pretty-print a field type *)

(** {3} Operations on values *)

val getf : 'm msg -> ('m, 'a) field -> 'a
(** Read a field of a message *)

val setf : 'm msg -> ('m, 'a) field -> 'a -> unit
(** Write a field of a message *)

val pp_field : 'a field_type -> Format.formatter -> 'a -> unit
(** Pretty-print a field value *)

val pp_msg : 'a msgtype -> Format.formatter -> 'a msg -> unit
(** Pretty-print a message *)

val create : 'm msgtype -> 'm msg
(** [create mt] creates a message of the type represented by [mt]. *)

val read : 'm msgtype -> 'm msg Angstrom.t
(** [read mt] returns an Angstrom parser for a message described by [mt]. *)

val write : _ msg -> Faraday.t
(** [write m] builds a Faraday serializer for [m]. *)

exception Parse_error
