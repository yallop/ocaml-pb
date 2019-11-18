(*
 * Copyright (c) 2017 Jeremy Yallop <yallop@docker.com>.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Protobuf wire types *)

open Unsigned

exception Parse_error

module Wire_type = Wire_type

type key = {
  field_number: uint64;
  wire_type: Wire_type.t
}

module KeyMap = MoreLabels.Map.Make(struct
    type t = key
    let compare a b =
      match UInt64.compare a.field_number b.field_number with
      | 0 -> compare a.wire_type b.wire_type
      | n -> n
  end)

type 'a enum = Enum_constant of int32

type _ field_type =
  | Fixed64 : uint64 field_type
  | Sfixed64 : int64 field_type
  | Double : float field_type
  | String : string field_type
  | Bytes : bytes field_type
  | Fixed32 : uint32 field_type
  | Sfixed32 : int32 field_type
  | Float : float field_type
  | Varint : uint64 field_type
  | Int32 : int32 field_type
  | Int64 : int64 field_type
  | Uint32 : uint32 field_type
  | Uint64 : uint64 field_type
  | Sint32 : int32 field_type
  | Sint64 : int64 field_type
  | Bool : bool field_type
  | Msg : 'm msgtype -> 'm msg field_type
  | Enum : (int32 * string) list ref -> 'e enum field_type
and ('a, 'b) field_kind =
    Optional : {default:'a option} -> ('a, 'a option) field_kind
  | Repeated : {packed:bool} -> ('a, 'a list) field_kind
  | Required: ('a, 'a) field_kind
and ('m,'a) field = Field : {
  key: key;
  field_type: 'a field_type;
  name: string;
  field_kind: ('a, 'b) field_kind
} -> ('m, 'b) field
and 'm boxed_field = Boxed_field : ('m, 'a) field -> 'm boxed_field
and 'm msgtype = {
  msg_name: string;
  mutable fields: 'm boxed_field list;
}
and 'a msg = { mutable contents: string list KeyMap.t; }

let one_two_seven = UInt64.of_int 127

let write_varint : Faraday.t -> uint64 -> unit =
  fun f i ->
    let rec loop b =
      let byte = UInt64.to_int UInt64.Infix.(b land UInt64.of_int 0x7f) in
      if b > one_two_seven
      then begin
        Faraday.write_uint8 f (byte lor 128);
        loop UInt64.Infix.(b lsr 7);
      end
      else Faraday.write_uint8 f byte
    in loop i

let fixed64 = Fixed64
let sfixed64 = Sfixed64
let double = Double
let string = String (* UTF-8, but we don't decode it *)
let bytes = Bytes
let fixed32 = Fixed32
let sfixed32 = Sfixed32
let float = Float
let int32 = Int32
let int64 = Int64
let uint32 = Uint32 
let uint64 = Uint64
let sint32 = Sint32
let sint64 = Sint64
let bool = Bool
let msg mt = Msg mt

let write_int64 f v = write_varint f (UInt64.of_int64 v)

let write_int32 f v = write_int64 f (Int64.of_int32 v)

let write_fixed_int64 : Faraday.t -> int64 -> unit =
  fun f x -> Faraday.LE.write_uint64 f x

let write_fixed_uint64 : Faraday.t -> uint64 -> unit =
  fun f x -> Faraday.LE.write_uint64 f (UInt64.to_int64 x)

let write_bytes f b =
  write_int64 f (Int64.of_int (Bytes.length b));
  Faraday.write_bytes f b

let write_fixed_int32 : Faraday.t -> int32 -> unit =
  fun f x -> Faraday.LE.write_uint32 f x

let write_fixed_uint32 : Faraday.t -> uint32 -> unit =
  fun f x -> Faraday.LE.write_uint32 f (UInt32.to_int32 x)

(* Each key in the streamed message is a varint with the value
   (field_number << 3) | wire_type – in other words, the last three bits
   of the number store the wire type.  *)
let key_of_uint64 k =
  let open UInt64 in
  let wire_type = Infix.(Wire_type.of_int Infix.(to_int (k land of_int 0x7)))
  and field_number = Infix.(k lsr 3) in
  { field_number; wire_type }

let uint64_of_key { field_number; wire_type } =
  let open UInt64 in
  Infix.((field_number lsl 3) lor of_int (Wire_type.to_int wire_type))

module type ENUM = sig
  type e
  val t : e enum field_type
  val constant : string -> int32 -> e enum
end

module Enum(X: sig val name : string end) :
sig
  include ENUM
  val constants : (int32 * string) list ref
end =
struct
  type e = X
  let of_enum (Enum_constant c) = c
  let to_enum c = Enum_constant c
  let constants = ref []
  let t = Enum constants
  let constant s c =
    let () = constants := (c, s) :: !constants in
    (to_enum c)
end

let enum s = (module Enum(struct let name = s end) : ENUM)

let constant_value (Enum_constant i) = i

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

let rec write_field : type a.a field_type -> Faraday.t -> a -> unit =
  fun t f v -> match t with
    | Fixed64 -> write_fixed_uint64 f v
    | Sfixed64 -> write_fixed_int64 f v
    | Double -> write_fixed_int64 f (Int64.bits_of_float v)
    | String -> write_bytes f (Bytes.unsafe_of_string v)
    | Bytes -> write_bytes f v
    | Fixed32 -> write_fixed_uint32 f v
    | Sfixed32 -> write_fixed_int32 f v
    | Float -> write_fixed_int32 f (Int32.bits_of_float v)
    | Varint -> write_varint f v
    | Int32 -> write_int32 f v
    | Int64 -> write_int64 f v
    | Uint32 -> let v = UInt64.of_int64 (Int64.of_int32 (UInt32.to_int32 v)) in
                write_varint f v
    | Uint64 -> write_varint f v
    | Sint32 -> let n = Signed.Int32.(Infix.((v lsl 1) lxor (shift_right v 31))) in
                write_int32 f n
    | Sint64 -> let n = Signed.Int64.(Infix.((v lsl 1) lxor (shift_right v 63))) in
                write_int64 f n
    | Bool -> if v then write_varint f UInt64.one
              else write_varint f UInt64.zero
    | Msg mt ->
      let f' = Faraday.create 128 in
      let () = write_contents f' v.contents in
      let s = Faraday.serialize_to_string f' in
      let () = write_varint f (UInt64.of_int (String.length s)) in
      Faraday.write_string f s
    | Enum _ -> let Enum_constant c = v in write_int32 f c

and write_contents : Faraday.t -> string list KeyMap.t -> unit =
  fun f contents ->
    KeyMap.iter contents ~f:(fun ~key:k ~data ->
      ListLabels.iter data ~f:(fun v ->
        write_varint f (uint64_of_key k);
        Faraday.write_string f v))

let string_of_field ft v =
  let fd = Faraday.create 128 in
  let () = write_field ft fd v in
  let s = Faraday.serialize_to_string fd in
  s

let read_varint : uint64 Angstrom.t =
  let open Angstrom in
  let add c (acc : uint64) shift =
    let shft = 7 * shift in
    UInt64.(Infix.(acc + (of_int c lsl shft)))
  in
  let rec loop (acc : uint64) shift =
    any_char >>= fun c ->
    if Char.code c land 0x80 > 0 then
      loop (add (Char.code c land 0x7f) acc shift) (succ shift)
    else return (add (Char.code c) acc shift)
  in loop UInt64.zero 0

let read_string : string Angstrom.t =
  Angstrom.(read_varint >>| UInt64.to_int >>= take)

let read_key =
  Angstrom.(read_varint >>| key_of_uint64)

let read_int32 =
  Angstrom.(read_varint >>| UInt64.to_int64 >>| Int64.to_int32)

let read_int64 =
  Angstrom.(read_varint >>| UInt64.to_int64)

let read_bytes =
  Angstrom.(read_string >>| Bytes.unsafe_of_string)

let read_unknown : Wire_type.t -> string Angstrom.t =
  let open Angstrom in
  let open Wire_type in
  function
  | Varint ->
    read_varint >>= fun v ->
    return (string_of_field Varint v)
  | Sixty_four -> 
    take 8 
  | Length_delimited ->
    read_bytes >>| string_of_field bytes
  | Start_group -> failwith "Not supported: start_group"
  | End_group -> failwith "Not supported: end_group"
  | Thirty_two ->
    take 4

let read_contents : string list KeyMap.t Angstrom.t =
  let add_one : string list KeyMap.t -> key -> string -> string list KeyMap.t =
    fun h k v ->
      match KeyMap.find k h with
      | exception Not_found -> KeyMap.add h ~key:k ~data:[v]
      | vs -> KeyMap.add h ~key:k ~data:(v :: vs)
  in
  let map_of_items items =
    List.fold_left (fun a (k, s) -> add_one a k s) items
  in
  let open Angstrom in
  let read_kv =
    read_key >>= fun k ->
    read_unknown k.wire_type >>= fun v ->
    return (k, v)
  in
  many read_kv >>| map_of_items KeyMap.empty
  
let read_field : type a. a field_type -> a Angstrom.t = 
  let open Angstrom in function
    | Fixed64 -> LE.any_int64 >>| UInt64.of_int64
    | Sfixed64 -> LE.any_int64
    | Double -> LE.any_int64 >>| Int64.float_of_bits
    | String -> read_string
    | Bytes -> read_bytes
    | Fixed32 -> LE.any_int32 >>| UInt32.of_int32
    | Sfixed32 -> LE.any_int32
    | Float -> LE.any_int32 >>| Int32.float_of_bits
    | Varint -> read_varint
    | Int32 -> read_int32
    | Int64 -> read_varint >>| UInt64.to_int64
    | Uint32 -> read_varint >>| UInt64.to_int64
                              >>| Int64.to_int32
                              >>| UInt32.of_int32
    | Uint64 -> read_varint
    | Sint32 -> read_int32 >>| fun n ->
                Signed.Int32.(Infix.((n lsr 1) lxor neg (n land 1l)))
    | Sint64 -> read_int64 >>| fun n ->
                Signed.Int64.(Infix.((n lsr 1) lxor neg (n land 1L)))
    | Bool -> read_varint >>| fun i ->
              not (i = UInt64.zero)
    | Msg mt -> read_string >>| fun s ->
                begin match Angstrom.parse_string read_contents s with
                  Result.Ok contents -> { contents }
                | Result.Error _ -> raise Parse_error
                end
    | Enum _ -> read_int32 >>| fun i ->
                Enum_constant i

let wire_type : type a. a field_type -> Wire_type.t =
  let open Wire_type in function
    | Fixed64 -> Sixty_four
    | Sfixed64 -> Sixty_four
    | Double -> Sixty_four

    | String -> Length_delimited
    | Bytes -> Length_delimited
    | Msg mt -> Length_delimited

    | Fixed32 -> Thirty_two
    | Sfixed32 -> Thirty_two
    | Float -> Thirty_two

    | Varint -> Varint
    | Int32 -> Varint
    | Int64 -> Varint
    | Uint32 -> Varint
    | Uint64 -> Varint
    | Sint32 -> Varint
    | Sint64 -> Varint
    | Bool -> Varint
    | Enum _ -> Varint

module Message (X: sig val name: string end) : MESSAGE =
struct
  (** a message is a key↦value map *) 
  type m = M

  let t = { msg_name = X.name; fields = [] }

  let field field_kind field_type name field_number =
    (* TODO: check for duplicate field numbers *)
    Field { key = { field_number = UInt64.of_int field_number;
                    wire_type = wire_type field_type };
            name; field_type; field_kind }

  (* TODO: handle 'packed' *)
  let repeated ?(packed=false) ft name n =
    let f = field (Repeated {packed}) ft name n in
    (* TODO: handle 'default' *)
    t.fields <- Boxed_field f :: t.fields;
    f

  let optional ?default ft name n =
    let f = field (Optional {default}) ft name n in
    t.fields <- Boxed_field f :: t.fields;
    f
    
  let required ft name n =
    let f = field Required ft name n in
    t.fields <- Boxed_field f :: t.fields;
    f
end

let create msg_type = { contents = KeyMap.empty }

let dump : string -> Format.formatter -> unit =
  let module PP = struct
    let reads (t : _ field_type) s =
      match Angstrom.parse_string (read_field t) s with
      | Result.Error s -> raise Parse_error
      | Result.Ok v -> v
    let fprintf = Format.fprintf
    let rec msg fmt m : unit =
      fprintf fmt "{@[@ ";
      KeyMap.iter m ~f:(fun ~key:k ~data ->
          fprintf fmt "@[@[%s@]@ =>@ @[%a@]@],@ "
            (UInt64.to_string k.field_number)
            fields (k.wire_type, data));
      fprintf fmt "@]}"
    and list fmt f l =
      fprintf fmt "[@[";
      let len = List.length l in
      ListLabels.iteri l ~f:(fun i x ->
        fprintf fmt "@[";
        f fmt x;
        if succ i < len then fprintf fmt ",@;";
        fprintf fmt "@]";
      );
      fprintf fmt "]@]"
    and fields fmt (wire_type, (fs : string list)) =
      let open Wire_type in
      match wire_type with
      | Varint -> list fmt varint fs
      | Sixty_four -> list fmt sixty_four fs
      | Length_delimited -> list fmt length_delimited fs
      | Start_group
      | End_group -> fprintf fmt "@[<groups not supported>@]"
      | Thirty_two -> list fmt thirty_two fs
    and varint fmt s =
      let i64 = reads int64 s
      and _u64 = reads uint64 s in
      fprintf fmt "%Ld" i64
    and sixty_four fmt s =
      (** fixed64, sfixed64, double *)
      let _f64 = reads fixed64 s
      and sf64 = reads sfixed64 s
      and _d = reads double s in
      fprintf fmt "%Ld" sf64
    and thirty_two fmt s =
    (** fixed32, sfixed32, float *)
      let _f32 = reads fixed32 s
      and sf32 = reads sfixed32 s
      and _f = reads float s in
      fprintf fmt "%ld" sf32
    and length_delimited fmt s =
    (** string, bytes, embedded messages, packed repeated fields *)
      let b = reads string s in
      match Angstrom.parse_string read_contents s with
      | Result.Ok m -> msg fmt m
      | Result.Error _ -> Format.fprintf fmt "@[%S@]" b
  end in
  fun s fmt ->
  match Angstrom.parse_string read_contents s with
  | Result.Ok m -> Format.fprintf fmt "@[%a@]@." PP.msg m
  | Result.Error _ -> raise Parse_error

let message name =
  (module Message (struct let name = name end) : MESSAGE)

let read_from_string p s =
  match Angstrom.parse_string p s with
  | Result.Error _ -> raise Parse_error
  | Result.Ok v -> v

let getf : type m a. m msg -> (m, a) field -> a =
  fun msg (Field field) -> match field.field_kind with
    | Repeated _ ->
      begin match KeyMap.find field.key msg.contents with
      | exception Not_found -> []
      | v -> List.rev_map (fun s -> read_from_string (read_field field.field_type) s) v
      end
    | Optional _ ->
      begin match KeyMap.find field.key msg.contents with
      | exception Not_found -> None
      | [] -> None
      | h :: _ -> Some (read_from_string (read_field field.field_type) h)
      end
    | Required ->
      begin match KeyMap.find field.key msg.contents with
      | [] -> raise Not_found
      | h :: _ -> read_from_string (read_field field.field_type) h
      end

let setf : type m a. m msg -> (m, a) field -> a -> unit =
  fun msg (Field field) v ->
    match field.field_kind, v with
    | Repeated _, v ->
      let fields = List.map (string_of_field field.field_type) v in
      msg.contents <- KeyMap.add msg.contents ~key:field.key ~data:fields
    | Optional _, None ->
      msg.contents <- KeyMap.add msg.contents ~key:field.key ~data:[]
    | Optional _, Some v ->
      let fields = string_of_field field.field_type v in
      msg.contents <- KeyMap.add msg.contents ~key:field.key ~data:[fields]
    | Required, v ->
      let fields = string_of_field field.field_type v in
      msg.contents <- KeyMap.add msg.contents ~key:field.key ~data:[fields]

(** Pretty-printing *)
let pp_field_type : type a. Format.formatter -> a field_type -> unit =
  let pp_string = Format.pp_print_string in
  fun fmt -> function
  | Fixed64 -> pp_string fmt "fixed64"
  | Sfixed64 -> pp_string fmt "sfixed64"
  | Double -> pp_string fmt "double"
  | String -> pp_string fmt "string"
  | Bytes -> pp_string fmt "bytes"
  | Fixed32 -> pp_string fmt "fixed32"
  | Sfixed32 -> pp_string fmt "sfixed32"
  | Float -> pp_string fmt "float"
  | Varint -> pp_string fmt "varint"
  | Int32 -> pp_string fmt "int32"
  | Int64 -> pp_string fmt "int64"
  | Uint32 -> pp_string fmt "uint32"
  | Uint64 -> pp_string fmt "uint64"
  | Sint32 -> pp_string fmt "sint32"
  | Sint64 -> pp_string fmt "sint64"
  | Bool -> pp_string fmt "bool"
  | Msg {msg_name} ->
    (* TODO: print fields *)
    Format.fprintf fmt "Message:%s" msg_name
  | Enum _ -> pp_string fmt "<enum>"

module PP =
struct
  let pp_key fmt { field_number; wire_type } =
    Format.fprintf fmt "@[{@ field_number@ =@ %s;@ wire_type@ =@ %s}@]"
      (UInt64.to_string field_number) (Wire_type.to_string wire_type)

  let rec pp_field : type a. a field_type -> Format.formatter -> a -> unit =
    fun ft fmt v -> match ft with
      | Fixed64 -> Format.pp_print_string fmt (UInt64.to_string v)
      | Sfixed64 -> Format.fprintf fmt "%Ld" v
      | Double -> Format.fprintf fmt "%g" v
      | String -> Format.pp_print_string fmt v
      | Bytes -> Format.pp_print_string fmt (Bytes.to_string v)
      | Fixed32 -> Format.pp_print_string fmt (UInt32.to_string v)
      | Sfixed32 -> Format.fprintf fmt "%ld" v
      | Float -> Format.fprintf fmt "%g" v
      | Varint -> Format.pp_print_string fmt (UInt64.to_string v)
      | Int32 -> Format.fprintf fmt "%ld" v
      | Int64 -> Format.fprintf fmt "%Ld" v
      | Uint32 -> Format.pp_print_string fmt (UInt32.to_string v)
      | Uint64 -> Format.pp_print_string fmt (UInt64.to_string v)
      | Sint32 -> Format.fprintf fmt "%ld" v
      | Sint64 -> Format.fprintf fmt "%Ld" v
      | Bool -> Format.pp_print_bool fmt v
      | Msg msg_type -> pp_msg msg_type fmt v
      | Enum constants ->
         let Enum_constant c = v in
         match List.assoc c !constants with
         | s -> Format.pp_print_string fmt s
         | exception Not_found -> Format.fprintf fmt "%ld" c
  and pp_msg : type m.m msgtype -> Format.formatter -> m msg -> unit =
    fun msg_type fmt v ->
      Format.fprintf fmt "@[%s@;{@[%a@]}@]" msg_type.msg_name (pp_fields msg_type) v
  and pp_fields : type m.m msgtype -> Format.formatter -> m msg -> unit =
    fun msg_type fmt {contents=tbl} ->
    begin
      Format.fprintf fmt "@[<hv>";
      let last_field = pred (KeyMap.cardinal tbl) in
      let fields = List.sort
          (fun ({field_number=l},_) ({field_number=r},_) ->
             compare l r) @@
        KeyMap.fold tbl ~init:[]
          ~f:(fun ~key:k ~data:v -> List.cons (k,v))
      in
      ListLabels.iteri fields ~f:begin fun j (key, data) ->
        begin match ListLabels.find msg_type.fields
                ~f:(function Boxed_field (Field {key=k}) ->
                    (* TODO: check wire types match *)
                    key.field_number = k.field_number) with
        | exception Not_found ->
          (* No information available: dump the string.
             TODO: perhaps also print 'unknown field' *)
          Format.fprintf fmt "@[%a@ =>@ [@[" pp_key key;
          ListLabels.iter data ~f:(Format.fprintf fmt "@[%S,@]@ ");
          Format.fprintf fmt "]@]@]";
        | Boxed_field (Field {field_type=ft; name}) ->
          (* We have field information available *) 
          let last = pred (List.length data) in
          begin match data with
              [] -> ()
            | [d] ->
              Format.fprintf fmt "@[%s@ =>@ %a"
                             name
                             (pp_field ft) (read_from_string (read_field ft) d);
              if j < last_field then Format.fprintf fmt ",@]@ "
              else Format.fprintf fmt "@]"
            | data ->
              Format.fprintf fmt "@[%s@ =>@ [@[" name;
              ListLabels.iteri data
                ~f:(fun i d ->
                    if i < last then
                      Format.fprintf fmt "@[%a,@]@ " (pp_field ft) (read_from_string (read_field ft) d)
                    else
                      Format.fprintf fmt "@[%a@]@," (pp_field ft) (read_from_string (read_field ft) d)
                  );
              if j < last_field then Format.fprintf fmt "],@]@]@;"
              else Format.fprintf fmt "]@]@]@;"
          end
        end;
      end;
      Format.fprintf fmt "@]";
    end

end

let pp_field = PP.pp_field
let pp_msg = PP.pp_msg

(* Read a top-level message (not an embedded message) *)
let read _ = Angstrom.(read_contents >>| fun contents -> { contents })

(* Write a top-level message (not an embedded message) *)
let write m = 
  let f = Faraday.create 1024 in write_contents f m.contents; f
