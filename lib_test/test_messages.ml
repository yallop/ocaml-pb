open Pb

module Enum =
struct
  module E = (val enum "Enum")
  let one = E.constant "one" 1_l
  let two = E.constant "two" 2_l
end

module Small =
struct
  module S = (val message "Small")
  let s = S.optional string "small_s" 100
  let i = S.optional int64  "small_i" 200
end

module TwoString =
struct
  module T = (val message "TwoString")
  let s = T.required string "two_s" 1000
  let b = T.required string "two_b" 2000
end

module Comprehensive =
struct
  module C = (val message "Comprehensive")
  let repeated_uint32   =
    C.repeated uint32          "repeated_uint32"   1
  let required_int32    =
    C.required int32           "required_int32"    2
  let required_Small    =
    C.required (msg Small.S.t) "required_Small"    3
  let required_double   =
    C.required double          "required_double"   4
  let optional_sfixed32 =
    C.optional sfixed32        "optional_sfixed32" 5
  let optional_fixed32  =
    C.optional fixed32         "optional_fixed32"  6
  let repeated_bytes    =
    C.repeated bytes           "repeated_bytes"    7
  let repeated_bool     =
    C.repeated bool            "repeated_bool"     8
  let repeated_sfixed64 =
    C.repeated sfixed64        "repeated_sfixed64" 9
  let optional_bool     =
    C.optional bool            "optional_bool"     10
  let required_uint32   =
    C.required uint32          "required_uint32"   11
  let optional_double   =
    C.optional double          "optional_double"   12
  let required_int64    =
    C.required int64           "required_int64"    13
  let required_uint64   =
    C.required uint64          "required_uint64"   14
  let required_string   =
    C.required string          "required_string"   15
  let required_bytes    =
    C.required bytes           "required_bytes"    16
  let optional_bytes    =
    C.optional bytes           "optional_bytes"    17
  let optional_sint64   =
    C.optional sint64          "optional_sint64"   18
  let repeated_sint64   =
    C.repeated sint64          "repeated_sint64"   19
  let repeated_fixed32  =
    C.repeated fixed32         "repeated_fixed32"  20
  let optional_Small    =
    C.optional (msg Small.S.t) "optional_Small"    21
  let optional_int32    =
    C.optional int32           "optional_int32"    22
  let optional_fixed64  =
    C.optional fixed64         "optional_fixed64"  23
  let optional_enum     =
    C.optional Enum.E.t        "optional_enum"     24
  let required_float    =
    C.required float           "required_float"    25
  let optional_sfixed64 =
    C.optional sfixed64        "optional_sfixed64" 26
  let required_sfixed32 =
    C.required sfixed32        "required_sfixed32" 27
  let required_bool     =
    C.required bool            "required_bool"     28
  let repeated_fixed64  =
    C.repeated fixed64         "repeated_fixed64"  29
  let optional_sint32   =
    C.optional sint32          "optional_sint32"   30
  let repeated_int64    =
    C.repeated int64           "repeated_int64"    31
  let required_fixed64  =
    C.required fixed64         "required_fixed64"  32
  let repeated_enum     =
    C.repeated Enum.E.t        "repeated_enum"     33
  let optional_int64    =
    C.optional int64           "optional_int64"    34
  let repeated_float    =
    C.repeated float           "repeated_float"    35
  let repeated_sint32   =
    C.repeated sint32          "repeated_sint32"   36
  let repeated_uint64   =
    C.repeated uint64          "repeated_uint64"   37
  let repeated_Small    =
    C.repeated (msg Small.S.t) "repeated_Small"    38
  let repeated_double   =
    C.repeated double          "repeated_double"   39
  let repeated_string   =
    C.repeated string          "repeated_string"   40
  let required_sfixed64 =
    C.required sfixed64        "required_sfixed64" 41
  let required_sint64   =
    C.required sint64          "required_sint64"   42
  let optional_string   =
    C.optional string          "optional_string"   43
  let optional_uint32   =
    C.optional uint32          "optional_uint32"   44
  let repeated_sfixed32 =
    C.repeated sfixed32        "repeated_sfixed32" 45
  let optional_float    =
    C.optional float           "optional_float"    46
  let optional_uint64   =
    C.optional uint64          "optional_uint64"   47
  let required_enum     =
    C.required Enum.E.t        "required_enum"     48
  let required_sint32   =
    C.required sint32          "required_sint32"   49
  let required_fixed32  =
    C.required fixed32         "required_fixed32"  50
  let repeated_int32    =
    C.repeated int32           "repeated_int32"    51
end
