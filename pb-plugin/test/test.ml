(*
 * Copyright (c) 2017 Jeremy Yallop <yallop@docker.com>.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
module C = Comprehensive

let msg_to_string m = Faraday.serialize_to_string (Pb.write m)

let read_from_string p s =
  match Angstrom.parse_string p s with
  | Result.Error _ -> Printf.kprintf failwith "parse failure (%s)" s
  | Result.Ok v -> v

let slurp filename = BatFile.with_file_in filename BatIO.read_all

let spew s filename = BatFile.with_file_out filename @@ fun fd ->
  BatIO.write_string fd s


let test_interoperability_small_read _ =
  let open C.Small in
  let small_string = slurp "small.python.serialized" in
  let small = extract
      (read_from_string (Pb.read C.Types_.Small.t) small_string) in
  begin
    assert (small.small_s = Some "abc");
    assert (small.small_i = Some 17L);
  end


let test_interoperability_comprehensive_read _ =
  let open C.Small in
  let open C.Comprehensive in
  let comprehensive_string = slurp "comprehensive.python.serialized" in
  let c = extract (read_from_string (Pb.read C.Types_.Comprehensive.t)
                     comprehensive_string) in
  begin
    let u32 = Unsigned.UInt32.of_int
    and u64 = Unsigned.UInt64.of_int
    and bytes = Bytes.of_string in
    assert (c.repeated_uint32 = [u32 1; u32 2]);
    assert (c.required_int32 = 3_l);
    let s = c.required_Small in
    assert (s.small_s = Some "abc");
    assert (s.small_i = Some 17L);
    assert (c.required_double = 4.1);
    assert (c.optional_sfixed32 = Some 5_l);
    assert (c.optional_fixed32 = Some (u32 6));
    assert (c.repeated_bytes = [bytes "def"; bytes "gh"]);
    assert (c.repeated_bool = [false; true]);
    assert (c.repeated_sfixed64 = [7_L; 8_L; 9_L]);
    assert (c.optional_bool = Some true);
    assert (c.required_uint32 = u32 10);
    assert (c.optional_double = Some 11.2);
    assert (c.required_int64 = 12_L);
    assert (c.required_uint64 = u64 13);
    assert (c.required_string = "rstuvw");
    assert (c.required_bytes = bytes "lmnopq");
    assert (c.optional_bytes = Some (bytes "rstuv"));
    assert (c.optional_sint64 = Some 14_L);
    assert (c.repeated_sint64 = [-15_L; 16_L; 17_L]);
    assert (c.repeated_fixed32 = [u32 18; u32 19; u32 20; u32 21]);
    let () = match c.optional_Small with
        None -> assert false
      | Some s ->
        assert (s.small_s = Some "abc");
        assert (s.small_i = Some 17L);
    in
    assert (c.optional_int32 = Some 22_l);
    assert (c.optional_fixed64 = Some (u64 23));
    assert (c.optional_enum = Some C.Enum.one);
    assert (c.required_float = 24.5);
    assert (c.optional_sfixed64 = Some 25_L);
    assert (c.required_sfixed32 = 26_l);
    assert (c.required_bool = true);
    assert (c.repeated_fixed64 = [u64 27; u64 28; u64 29; u64 30; u64 31]);
    assert (c.optional_sint32 = Some 32_l);
    assert (c.repeated_int64 = [33_L; 34_L; 35_L; 36_L; 37_L; 38_L; 39_L]);
    assert (c.required_fixed64 = u64 40);
    assert (c.repeated_enum = [C.Enum.one;  C.Enum.two]);
    assert (c.optional_int64 = Some 41_L);
    assert (c.repeated_float = [42.0]);
    assert (c.repeated_sint32 = [44_l; 45_l; 46_l; 47_l; 48_l; 49_l]);
    assert (c.repeated_uint64 = [u64 50; u64 51; u64 52; u64 53; u64 54; u64 55]);
    let () = match c.repeated_Small with
        [s1; s2] ->
        assert (s2.small_s = None);
        assert (s2.small_i = Some 100L);
        assert (s1.small_s = Some "abc");
        assert (s1.small_i = Some 17L);
      | _ -> assert false
    in
    assert (c.repeated_double = [56.3; 57.4; 58.0; 59.1]);
    assert (c.repeated_string = ["w"; ""; "yz"]);
    assert (c.required_sfixed64 = 60_L);
    assert (c.required_sint64 = 61_L);
    assert (c.optional_string = Some "A3");
    assert (c.optional_uint32 = Some (u32 62));
    assert (c.repeated_sfixed32 = [63_l; 64_l; 65_l; 66_l; 67_l; 68_l]);
    assert (c.optional_float = Some 69.0);
    assert (c.optional_uint64 = Some (u64 70));
    assert (c.required_enum = C.Enum.two);
    assert (c.required_sint32 = 71_l);
    assert (c.required_fixed32 = u32 72);
    assert (c.repeated_int32 = [73_l; 74_l; 75_l; 76_l; 77_l; 78_l]);
 end


let test_interoperability_small_write _ =
  let small = C.Small.mk ~small_s:"abc" ~small_i:17_L () in
  spew (msg_to_string small) "small.ocaml.serialized"


let test_interoperability_comprehensive_write _ =
  let u32 = Unsigned.UInt32.of_int
  and u64 = Unsigned.UInt64.of_int and bytes = Bytes.of_string in
  let s = C.Small.mk ~small_s:"abc" ~small_i:17_L () in
  let s2 = C.Small.mk ~small_i:100_L () in
  let c = C.Comprehensive.mk
    ~repeated_uint32:[u32 1; u32 2]
    ~required_int32:3_l
    ~required_Small:s
    ~required_double:4.1
    ~optional_sfixed32:5_l
    ~optional_fixed32:(u32 6)
    ~repeated_bytes:[bytes "def"; bytes "gh"]
    ~repeated_bool:[false; true]
    ~repeated_sfixed64:[7_L; 8_L; 9_L]
    ~optional_bool:true
    ~required_uint32:(u32 10)
    ~optional_double:11.2
    ~required_int64:12_L
    ~required_uint64:(u64 13)
    ~required_string:"rstuvw"
    ~required_bytes:(bytes "lmnopq")
    ~optional_bytes:(bytes "rstuv")
    ~optional_sint64:14_L
    ~repeated_sint64:[-15_L; 16_L; 17_L]
    ~repeated_fixed32:[u32 18; u32 19; u32 20; u32 21]
    ~optional_Small:s
    ~optional_int32:22_l
    ~optional_fixed64:(u64 23)
    ~optional_enum:C.Enum.one
    ~required_float:24.5
    ~optional_sfixed64:25_L
    ~required_sfixed32:26_l
    ~required_bool:true
    ~repeated_fixed64:[u64 27; u64 28; u64 29; u64 30; u64 31]
    ~optional_sint32:32_l
    ~repeated_int64:[33_L; 34_L; 35_L; 36_L; 37_L; 38_L; 100000000039_L]
    ~required_fixed64:(u64 40)
    ~repeated_enum:[C.Enum.one;  C.Enum.two]
    ~optional_int64:41_L
    ~repeated_float:[42.0]
    ~repeated_sint32:[44_l; 45_l; 46_l; 47_l; 48_l; 49_l]
    ~repeated_uint64:[u64 50; u64 51; u64 52; u64 53; u64 54; u64 55]
    ~repeated_Small:[s; s2]
    ~repeated_double:[56.3; 57.4; 58.0; 59.1]
    ~repeated_string:["w"; ""; "yz"]
    ~required_sfixed64:60_L
    ~required_sint64:61_L
    ~optional_string:"A3"
    ~optional_uint32:(u32 62)
    ~repeated_sfixed32:[63_l; 64_l; 65_l; 66_l; 67_l; 68_l]
    ~optional_float:69.0
    ~optional_uint64:(u64 70)
    ~required_enum:C.Enum.two
    ~required_sint32:71_l
    ~required_fixed32:(u32 72)
    ~repeated_int32:[73_l; 74_l; 75_l; 76_l; 77_l; 78_l]
    ()
  in
  spew (msg_to_string c) "comprehensive.ocaml.serialized"


let suite = "Protobuf wire format tests" >:::
  ["test interoperability (small read)"
   >:: test_interoperability_small_read;

  "test interoperability (comprehensive read)"
   >:: test_interoperability_comprehensive_read;

   "test interoperability (small write)"
   >:: test_interoperability_small_write;

   "test interoperability (comprehensive write)"
   >:: test_interoperability_comprehensive_write;
  ]


let _ =
  run_test_tt_main suite
