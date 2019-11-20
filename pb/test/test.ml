(*
 * Copyright (c) 2017 Jeremy Yallop <yallop@docker.com>.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Unsigned
open Pb
open Test_messages

let to_string t v =
  let f = Faraday.create 128 in
  write_field t f v;
  Faraday.serialize_to_string f

let msg_to_string m = Faraday.serialize_to_string (write m)

let read_from_string p s =
  match Angstrom.parse_string p s with
  | Result.Error _ -> Printf.kprintf failwith "parse failure (%s)" s
  | Result.Ok v -> v

let of_string t s = read_from_string (read_field t) s

let slurp filename =
  let {Unix.st_size=bytes;_} = Unix.stat filename in
  let fd = open_in filename in
  let s = really_input_string fd bytes in
  close_in fd;
  s

let spew s filename =
  let fd = open_out filename in
  output_string fd s;
  close_out fd

let test_roundtrip _ =
  let check_roundtrip ?printer ?msg t v =
    (* A comparison function that respects NaN's is necessary. (=) does not work
    because:
     compare nan nan = 0
     (nan = nan) = false *)
    assert_equal ?printer ?msg v (of_string t (to_string t v))
      ~cmp:(fun x y -> compare x y = 0)
  in
  begin
    check_roundtrip bool true;
    check_roundtrip bool false;

    (* Signed 32-bit integer encodings *)
    check_roundtrip int32 Int32.zero
      ~msg:"roundtrip int32 zero";
    check_roundtrip int32 Int32.min_int
      ~msg:"roundtrip int32 min_int";
    check_roundtrip int32 Int32.max_int
      ~msg:"roundtrip int32 max_int";

    check_roundtrip sint32 Int32.zero
      ~msg:"roundtrip sint32 zero";
    check_roundtrip sint32 Int32.min_int
      ~msg:"roundtrip sint32 min_int"  ~printer:Int32.to_string;
    check_roundtrip sint32 Int32.max_int
      ~msg:"roundtrip sint32 max_int";

    check_roundtrip sfixed32 Int32.zero
      ~msg:"roundtrip sfixed32 zero";
    check_roundtrip sfixed32 Int32.min_int
      ~msg:"roundtrip sfixed32 min_int";
    check_roundtrip sfixed32 Int32.max_int
      ~msg:"roundtrip sfixed32 max_int";

    (* Signed 64-bit integer encodings *)
    check_roundtrip int64 Int64.zero
      ~msg:"roundtrip int64 zero";
    check_roundtrip int64 Int64.min_int
      ~msg:"roundtrip int64 min_int";
    check_roundtrip int64 Int64.max_int
      ~msg:"roundtrip int64 max_int";

    check_roundtrip sint64 Int64.zero
      ~msg:"roundtrip sint64 zero";
    check_roundtrip sint64 Int64.min_int
      ~msg:"roundtrip sint64 min_int";
    check_roundtrip sint64 Int64.max_int
      ~msg:"roundtrip sint64 max_int";

    check_roundtrip sfixed64 Int64.zero
      ~msg:"roundtrip sfixed64 zero";
    check_roundtrip sfixed64 Int64.one
      ~msg:"roundtrip sfixed64 one";
    check_roundtrip sfixed64 128L
      ~msg:"roundtrip sfixed64 128";
    check_roundtrip sfixed64 (-1L)
      ~msg:"roundtrip sfixed64 -1";
    check_roundtrip sfixed64 (-128L)
      ~msg:"roundtrip sfixed64 -128";
    check_roundtrip sfixed64 Int64.min_int
      ~msg:"roundtrip sfixed64 min_int";
    check_roundtrip sfixed64 Int64.max_int
      ~msg:"roundtrip sfixed64 max_int";


    (* Unsigned 32-bit integer encodings *)
    check_roundtrip uint32 UInt32.zero
      ~msg:"roundtrip uint32 zero"; 
    check_roundtrip uint32 UInt32.one
      ~msg:"roundtrip uint32 one"; 
    check_roundtrip uint32 UInt32.max_int
      ~msg:"roundtrip uint32 max_int"; 

    check_roundtrip fixed32 UInt32.zero
      ~msg:"roundtrip fixed32 zero"; 
    check_roundtrip fixed32 UInt32.one
      ~msg:"roundtrip fixed32 one"; 
    check_roundtrip fixed32 UInt32.max_int
      ~msg:"roundtrip fixed32 max_int"; 


    (* Unsigned 64-bit integer encodings *)
    check_roundtrip uint64 UInt64.zero
      ~msg:"roundtrip uint64 zero"; 
    check_roundtrip uint64 UInt64.one
      ~msg:"roundtrip uint64 one"; 
    check_roundtrip uint64 UInt64.max_int
      ~msg:"roundtrip uint64 max_int"; 

    check_roundtrip fixed64 UInt64.zero
      ~msg:"roundtrip fixed64 zero"; 
    check_roundtrip fixed64 UInt64.one
      ~msg:"roundtrip fixed64 one"; 
    check_roundtrip fixed64 UInt64.max_int
      ~msg:"roundtrip fixed64 max_int"; 

    (* Floating point encodings *)
    check_roundtrip ~printer:string_of_float double 0.0
      ~msg:"roundtrip double 0.0";
    check_roundtrip ~printer:string_of_float double 1.0
      ~msg:"roundtrip double 1.0";
    check_roundtrip ~printer:string_of_float double nan
      ~msg:"roundtrip double nan";
    check_roundtrip ~printer:string_of_float double infinity
      ~msg:"roundtrip double infinity";

    check_roundtrip ~printer:string_of_float float 0.0
      ~msg:"roundtrip float 0.0";
    check_roundtrip ~printer:string_of_float float 1.0
      ~msg:"roundtrip float 1.0";
    check_roundtrip ~printer:string_of_float float nan
      ~msg:"roundtrip float nan";
    check_roundtrip ~printer:string_of_float float infinity
      ~msg:"roundtrip float infinity";


    (* Strings *)
    check_roundtrip string ""
      ~msg:"roundtrip string \"\""
      ~printer:(Printf.sprintf "%S");
    check_roundtrip string "abc"
      ~msg:"roundtrip string \"abc\"";
    check_roundtrip string "\000\001\002\003\004\005\006\007\b\t\n\011\012\r\014\015\016\017\018\019\020\021\022\023\024\025\026\027\028\029\030\031 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\127\128\129\130\131\132\133\134\135\136\137\138\139\140\141\142\143\144\145\146\147\148\149\150\151\152\153\154\155\156\157\158\159\160\161\162\163\164\165\166\167\168\169\170\171\172\173\174\175\176\177\178\179\180\181\182\183\184\185\186\187\188\189\190\191\192\193\194\195\196\197\198\199\200\201\202\203\204\205\206\207\208\209\210\211\212\213\214\215\216\217\218\219\220\221\222\223\224\225\226\227\228\229\230\231\232\233\234\235\236\237\238\239\240\241\242\243\244\245\246\247\248\249\250\251\252\253\254\255"
      ~msg:"roundtrip string \"(full range)\"";

    check_roundtrip bytes (Bytes.of_string "")
      ~msg:"roundtrip bytes \"\"";
    check_roundtrip bytes (Bytes.of_string "abc")
      ~msg:"roundtrip bytes \"abc\"";
    check_roundtrip bytes (Bytes.of_string "\000\001\002\003\004\005\006\007\b\t\n\011\012\r\014\015\016\017\018\019\020\021\022\023\024\025\026\027\028\029\030\031 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\127\128\129\130\131\132\133\134\135\136\137\138\139\140\141\142\143\144\145\146\147\148\149\150\151\152\153\154\155\156\157\158\159\160\161\162\163\164\165\166\167\168\169\170\171\172\173\174\175\176\177\178\179\180\181\182\183\184\185\186\187\188\189\190\191\192\193\194\195\196\197\198\199\200\201\202\203\204\205\206\207\208\209\210\211\212\213\214\215\216\217\218\219\220\221\222\223\224\225\226\227\228\229\230\231\232\233\234\235\236\237\238\239\240\241\242\243\244\245\246\247\248\249\250\251\252\253\254\255")
      ~msg:"roundtrip bytes \"(full range)\"";

    (* Messages *)
    let module M = struct
      module M1 = (val message "M1")
      let f1 = M1.repeated int32 "f1" 1
      let f2 = M1.repeated string "f2" 2

      module M2 = (val message "M2")
      let f4 = M2.repeated (msg M1.t) "f4" 15
      let f5 = M2.repeated sfixed64 "f5" 20
    end in
    let open M in

    let m1 = create M1.t in
    setf m1 f1 [77l];
    setf m1 f2 ["abc"; "def"];

    let s1 = to_string (msg M1.t) m1 in
    let m1' = of_string (msg M1.t) s1 in
    assert_equal [77l] (getf m1' f1);
    assert_equal ["abc"; "def"] (getf m1' f2);

    let m2 = create M2.t in
    setf m2 f5 [1001L];
    setf m2 f4 [m1];

    let s2 = to_string (msg M2.t) m2 in
    let m2' = of_string (msg M2.t) s2 in

    assert_equal [1001L] (getf m2' f5);
    let m1'' = List.hd (getf m2' f4) in
    assert_equal [77l] (getf m1'' f1);
    assert_equal ["abc"; "def"] (getf m1'' f2);
  end

let test_messages _ =
  let s = "\n\x06Robert" in
  let module Hello = (val message "Hello") in
  let name = Hello.required string "name" 1 in
  let m = read_from_string (read Hello.t) s in
  assert_equal "Robert" (getf m name)


let test_interoperability_small_read _ =
  let small_string = slurp "small.python.serialized" in
  let small = read_from_string (read Small.S.t) small_string in
  begin
    assert (getf small Small.s = Some "abc");
    assert (getf small Small.i = Some 17L);
  end


let test_interoperability_twostring_read _ =
  let two_string = slurp "twostring.python.serialized" in
  let two = read_from_string (read TwoString.T.t) two_string in
  begin
    assert (getf two TwoString.s = "abc");
    assert (getf two TwoString.b = "def");
  end

let test_interoperability_comprehensive_read _ =
  let comprehensive_string = slurp "comprehensive.python.serialized" in
  let c = read_from_string (read Comprehensive.C.t) comprehensive_string in
  begin
    let u32 = UInt32.of_int and u64 = UInt64.of_int and bytes = Bytes.of_string in
    assert (getf c Comprehensive.repeated_uint32 = [u32 1; u32 2]);
    assert (getf c Comprehensive.required_int32 = 3_l);
    let s = getf c Comprehensive.required_Small in
    assert (getf s Small.s = Some "abc");
    assert (getf s Small.i = Some 17L);
    assert (getf c Comprehensive.required_double = 4.1);
    assert (getf c Comprehensive.optional_sfixed32 = Some 5_l);
    assert (getf c Comprehensive.optional_fixed32 = Some (u32 6));
    assert (getf c Comprehensive.repeated_bytes = [bytes "def"; bytes "gh"]);
    assert (getf c Comprehensive.repeated_bool = [false; true]);
    assert (getf c Comprehensive.repeated_sfixed64 = [7_L; 8_L; 9_L]);
    assert (getf c Comprehensive.optional_bool = Some true);
    assert (getf c Comprehensive.required_uint32 = u32 10);
    assert (getf c Comprehensive.optional_double = Some 11.2);
    assert (getf c Comprehensive.required_int64 = 12_L);
    assert (getf c Comprehensive.required_uint64 = u64 13);
    assert (getf c Comprehensive.required_string = "rstuvw");
    assert (getf c Comprehensive.required_bytes = bytes "lmnopq");
    assert (getf c Comprehensive.optional_bytes = Some (bytes "rstuv"));
    assert (getf c Comprehensive.optional_sint64 = Some 14_L);
    assert (getf c Comprehensive.repeated_sint64 = [-15_L; 16_L; 17_L]);
    assert (getf c Comprehensive.repeated_fixed32 = [u32 18; u32 19; u32 20; u32 21]);
    let () = match getf c Comprehensive.optional_Small with
        None -> assert false
      | Some s ->
        assert (getf s Small.s = Some "abc");
        assert (getf s Small.i = Some 17L);
    in
    assert (getf c Comprehensive.optional_int32 = Some 22_l);
    assert (getf c Comprehensive.optional_fixed64 = Some (u64 23));
    assert (getf c Comprehensive.optional_enum = Some Enum.one);
    assert (getf c Comprehensive.required_float = 24.5);
    assert (getf c Comprehensive.optional_sfixed64 = Some 25_L);
    assert (getf c Comprehensive.required_sfixed32 = 26_l);
    assert (getf c Comprehensive.required_bool = true);
    assert (getf c Comprehensive.repeated_fixed64 = [u64 27; u64 28; u64 29; u64 30; u64 31]);
    assert (getf c Comprehensive.optional_sint32 = Some 32_l);
    assert (getf c Comprehensive.repeated_int64 = [33_L; 34_L; 35_L; 36_L; 37_L; 38_L; 39_L]);
    assert (getf c Comprehensive.required_fixed64 = u64 40);
    assert (getf c Comprehensive.repeated_enum = [Enum.one;  Enum.two]);
    assert (getf c Comprehensive.optional_int64 = Some 41_L);
    assert (getf c Comprehensive.repeated_float = [42.0]);
    assert (getf c Comprehensive.repeated_sint32 = [44_l; 45_l; 46_l; 47_l; 48_l; 49_l]);
    assert (getf c Comprehensive.repeated_uint64 = [u64 50; u64 51; u64 52; u64 53; u64 54; u64 55]);
    let () = match getf c Comprehensive.repeated_Small with
        [s1; s2] ->
        assert (getf s2 Small.s = None);
        assert (getf s2 Small.i = Some 100L);
        assert (getf s1 Small.s = Some "abc");
        assert (getf s1 Small.i = Some 17L);
      | _ -> assert false
    in
    assert (getf c Comprehensive.repeated_double = [56.3; 57.4; 58.0; 59.1]);
    assert (getf c Comprehensive.repeated_string = ["w"; ""; "yz"]);
    assert (getf c Comprehensive.required_sfixed64 = 60_L);
    assert (getf c Comprehensive.required_sint64 = 61_L);
    assert (getf c Comprehensive.optional_string = Some "A3");
    assert (getf c Comprehensive.optional_uint32 = Some (u32 62));
    assert (getf c Comprehensive.repeated_sfixed32 = [63_l; 64_l; 65_l; 66_l; 67_l; 68_l]);
    assert (getf c Comprehensive.optional_float = Some 69.0);
    assert (getf c Comprehensive.optional_uint64 = Some (u64 70));
    assert (getf c Comprehensive.required_enum = Enum.two);
    assert (getf c Comprehensive.required_sint32 = 71_l);
    assert (getf c Comprehensive.required_fixed32 = u32 72);
    assert (getf c Comprehensive.repeated_int32 = [73_l; 74_l; 75_l; 76_l; 77_l; 78_l]);
 end


let test_interoperability_small_write _ =
  let small = create Small.S.t in
  begin
    setf small Small.s (Some "abc");
    setf small Small.i (Some 17_L);
    spew (msg_to_string small) "small.ocaml.serialized";
  end
  

let test_interoperability_twostring_write _ =
  let two = create TwoString.T.t in
  begin
    setf two TwoString.s "abc";
    setf two TwoString.b "def";
    spew (msg_to_string two) "twostring.ocaml.serialized";
  end
  

let test_interoperability_comprehensive_write _ =
  let c = create Comprehensive.C.t in
  begin
    let u32 = UInt32.of_int and u64 = UInt64.of_int and bytes = Bytes.of_string in
    setf c Comprehensive.repeated_uint32 [u32 1; u32 2];
    setf c Comprehensive.required_int32 3_l;
    let s = create Small.S.t in
    setf s Small.s (Some "abc");
    setf s Small.i (Some 17L);
    setf c Comprehensive.required_Small s;
    setf c Comprehensive.required_double 4.1;
    setf c Comprehensive.optional_sfixed32 (Some 5_l);
    setf c Comprehensive.optional_fixed32 (Some (u32 6));
    setf c Comprehensive.repeated_bytes [bytes "def"; bytes "gh"];
    setf c Comprehensive.repeated_bool [false; true];
    setf c Comprehensive.repeated_sfixed64 [7_L; 8_L; 9_L];
    setf c Comprehensive.optional_bool (Some true);
    setf c Comprehensive.required_uint32 (u32 10);
    setf c Comprehensive.optional_double (Some 11.2);
    setf c Comprehensive.required_int64 12_L;
    setf c Comprehensive.required_uint64 (u64 13);
    setf c Comprehensive.required_string "rstuvw";
    setf c Comprehensive.required_bytes (bytes "lmnopq");
    setf c Comprehensive.optional_bytes (Some (bytes "rstuv"));
    setf c Comprehensive.optional_sint64 (Some 14_L);
    setf c Comprehensive.repeated_sint64 [-15_L; 16_L; 17_L];
    setf c Comprehensive.repeated_fixed32 [u32 18; u32 19; u32 20; u32 21];
    let s = create Small.S.t in
    setf s Small.s (Some "abc");
    setf s Small.i (Some 17L);
    setf c Comprehensive.optional_Small (Some s);
    setf c Comprehensive.optional_int32 (Some 22_l);
    setf c Comprehensive.optional_fixed64 (Some (u64 23));
    setf c Comprehensive.optional_enum (Some Enum.one);
    setf c Comprehensive.required_float 24.5;
    setf c Comprehensive.optional_sfixed64 (Some 25_L);
    setf c Comprehensive.required_sfixed32 26_l;
    setf c Comprehensive.required_bool true;
    setf c Comprehensive.repeated_fixed64 [u64 27; u64 28; u64 29; u64 30; u64 31];
    setf c Comprehensive.optional_sint32 (Some 32_l);
    setf c Comprehensive.repeated_int64 [33_L; 34_L; 35_L; 36_L; 37_L; 38_L; 100000000039_L];
    setf c Comprehensive.required_fixed64 (u64 40);
    setf c Comprehensive.repeated_enum [Enum.one;  Enum.two];
    setf c Comprehensive.optional_int64 (Some 41_L);
    setf c Comprehensive.repeated_float [42.0];
    setf c Comprehensive.repeated_sint32 [44_l; 45_l; 46_l; 47_l; 48_l; 49_l];
    setf c Comprehensive.repeated_uint64 [u64 50; u64 51; u64 52; u64 53; u64 54; u64 55];
    let s1 = create Small.S.t and s2 = create Small.S.t in
    setf s2 Small.s None;
    setf s2 Small.i (Some 100L);
    setf s1 Small.s (Some "abc");
    setf s1 Small.i (Some 17L);
    setf c Comprehensive.repeated_Small [s1; s2];
    setf c Comprehensive.repeated_double [56.3; 57.4; 58.0; 59.1];
    setf c Comprehensive.repeated_string ["w"; ""; "yz"];
    setf c Comprehensive.required_sfixed64 60_L;
    setf c Comprehensive.required_sint64 61_L;
    setf c Comprehensive.optional_string (Some "A3");
    setf c Comprehensive.optional_uint32 (Some (u32 62));
    setf c Comprehensive.repeated_sfixed32 [63_l; 64_l; 65_l; 66_l; 67_l; 68_l];
    setf c Comprehensive.optional_float (Some 69.0);
    setf c Comprehensive.optional_uint64 (Some (u64 70));
    setf c Comprehensive.required_enum Enum.two;
    setf c Comprehensive.required_sint32 71_l;
    setf c Comprehensive.required_fixed32 (u32 72);
    setf c Comprehensive.repeated_int32 [73_l; 74_l; 75_l; 76_l; 77_l; 78_l];
    spew (msg_to_string c) "comprehensive.ocaml.serialized";
  end


let suite = "Protobuf wire format tests" >:::
  ["test roundtrip"
   >:: test_roundtrip;

   "test messages"
   >:: test_messages;

   "test interoperability (small read)"
   >:: test_interoperability_small_read;

   "test interoperability (twostring read)"
   >:: test_interoperability_twostring_read;

  "test interoperability (comprehensive read)"
   >:: test_interoperability_comprehensive_read;

   "test interoperability (small write)"
   >:: test_interoperability_small_write;

   "test interoperability (twostring write)"
   >:: test_interoperability_twostring_write;

   "test interoperability (comprehensive write)"
   >:: test_interoperability_comprehensive_write;
  ]


let _ =
  run_test_tt_main suite
