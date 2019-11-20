(*
 * Copyright (c) 2017 Jeremy Yallop <yallop@docker.com>.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module M = Protoc_messages
module G = Protobuf_reified.Generate

(** Error handling *)
exception Error of string

let failf fmt = Printf.kprintf (fun s -> raise (Error s)) fmt
let warnf fmt = Printf.fprintf stderr ("Warning: " ^^ fmt ^^ "\n")
let is_some = function Some _ -> true | _ -> false
let iter_opt f = function Some x -> f x | None -> ()
let map_opt f = function Some x -> Some (f x) | None -> None

let basename s =
  let last = pred (String.length s) in
  match String.rindex_from s last '.' with
    i -> String.sub s (succ i) (last - i)
  | exception Not_found -> s

module Context :
sig
  type t
  val create : package:string option -> t

  val enum : t -> string -> G.enum
  (** hash-consed enums *)

  val message : t -> string -> G.message
  (** hash-consed messages *)

  val write_ml : Format.formatter -> t -> unit
  (** write out the contents of the context as an OCaml module *)
end =
struct
  type t = {
    package: string option;
    enums: (string, G.enum) Hashtbl.t;
    messages: (string, G.message) Hashtbl.t;
  }
  let create ~package =
    { package;
      messages = Hashtbl.create 10;
      enums = Hashtbl.create 10 }

  let message {messages; _} s =
    let s = basename s in
    match Hashtbl.find messages s with
    | m -> m
    | exception Not_found ->
       let m = G.message s in (Hashtbl.add messages s m; m)

  let enum {enums; _} s =
    let s = basename s in
    match Hashtbl.find enums s with
    | e -> e
    | exception Not_found ->
       let e = G.enum s in (Hashtbl.add enums s e; e)

  let write_ml fmt { package=_ ; enums ; messages } =
    (* TODO: package? *)
    let as_list h = Hashtbl.fold (fun _ -> List.cons) h [] in
    G.write fmt (as_list messages) (as_list enums)
end

module Enum :
sig
  val generate : Context.t -> M.EnumDescriptorProto.s -> unit
end =
struct
  module EVO = M.EnumValueOptions

  let handle_option { EVO.deprecated; uninterpreted_option=_ } =
    if deprecated
    then warnf "ignoring option deprecated";

  module EV = M.EnumValueDescriptorProto

  let genenum_value e = function
    | { EV.name = None; _ } -> failf "nameless enum values are not supported"
    | { EV.number = None; _ } -> failf "numberless enum values are not supported"
    | { EV.name = Some name; number = Some number; options } ->
       iter_opt handle_option options;
       G.constant name e number

  module E = M.EnumDescriptorProto

  let handle_option { M.EnumOptions.allow_alias; deprecated; uninterpreted_option=_ } =
    begin
      if is_some allow_alias
      then warnf "ignoring option allow_alias";

      if deprecated
      then warnf "ignoring option deprecated";
    end

  let generate context = function
    | { E.name = None; _ } -> failf "nameless enum values are not supported"
    | { E.name = Some name; value; options } ->
       iter_opt handle_option options;
       let e = Context.enum context name in
       List.iter (genenum_value e) value
end
  

module Message :
sig
  val generate : Context.t -> M.DescriptorProto.s -> unit
end =
struct
  let gentype context t name =
    match M.Type.T.of_value (Pb.constant_value t), name with
    | `TYPE_DOUBLE, _ -> G.double
    | `TYPE_FLOAT, _ -> G.float
    | `TYPE_INT64, _ -> G.int64
    | `TYPE_UINT64, _ -> G.uint64
    | `TYPE_INT32, _ -> G.int32
    | `TYPE_FIXED64, _ -> G.fixed64
    | `TYPE_FIXED32, _ -> G.fixed32
    | `TYPE_BOOL, _ -> G.bool
    | `TYPE_STRING, _ -> G.string
    | `TYPE_GROUP, _ -> failf "groups are not supported"
    | `TYPE_MESSAGE, None -> failf "message fields without names are not supported"
    | `TYPE_MESSAGE, Some m -> G.message_field (Context.message context m)
    | `TYPE_BYTES, _ -> G.bytes
    | `TYPE_UINT32, _ -> G.uint32
    | `TYPE_ENUM, None -> failf "enum fields without names are not supported"
    | `TYPE_ENUM, Some e -> G.enum_field (Context.enum context e)
    | `TYPE_SFIXED32, _ -> G.sfixed32
    | `TYPE_SFIXED64, _ -> G.sfixed64
    | `TYPE_SINT32, _ -> G.sint32
    | `TYPE_SINT64, _ -> G.sint64

  module FO = M.FieldOptions

  let handle_option { FO.ctype=_; FO.packed; FO.jstype=_; FO.lazy_; FO.deprecated; FO.weak;
                      FO.uninterpreted_option=_ } =
    begin
      if is_some packed
      then warnf "ignoring option packed";

      if is_some packed
      then warnf "ignoring option packed";

      if lazy_
      then warnf "ignoring option lazy";
      
      if deprecated
      then warnf "ignoring option deprecated";

      if weak
      then warnf "ignoring option weak";
    end

  module F = M.FieldDescriptorProto

  let genfield context msg = function
    | { F.name = None; _ } -> failf "nameless message fields not supported"
    | { F.number = None; _ } -> failf "numberless message fields not supported"
    | { F.type_ = None; _ } -> failf "typeless message fields not supported"
    | { F.name = Some name; number = Some number; label; type_ = Some type_; type_name;
        extendee; default_value; oneof_index; options; json_name=_ } as _r ->
       iter_opt handle_option options;
       begin
         if is_some extendee
         then warnf "ignoring extendee";

         if is_some oneof_index
         then warnf "ignoring oneof_index";
       end;
       let labelf = match label with
           None -> G.required
         | Some l -> match M.Label.T.of_value (Pb.constant_value l) with
                     | `LABEL_REQUIRED -> G.required
                     | `LABEL_REPEATED -> G.repeated
                     | `LABEL_OPTIONAL ->
                       begin match default_value with
                         | Some default -> G.optional ~default
                         | None -> G.optional ?default:None
                       end
       in
       let typ = gentype context type_ type_name in
       labelf name msg typ number

  module MO = M.MessageOptions

  let handle_options { MO.message_set_wire_format;
                       no_standard_descriptor_accessor;
                       deprecated: bool;
                       map_entry;
                       uninterpreted_option=_ } =
    begin
      if message_set_wire_format
      then warnf "ignoring option message_set_wire_format";

      if no_standard_descriptor_accessor
      then warnf "ignoring option no_standard_descriptor_accessor";

      if deprecated
      then warnf "ignoring option deprecated";

      if is_some map_entry
      then warnf "ignoring option map_entry";
    end

  module D = M.DescriptorProto

  let generate context = function
    | { D.name = None; _ } -> failf "nameless messages not supported"
    | { D.name = Some name; field; extension; nested_type; enum_type;
        extension_range; oneof_decl; options; reserved_range; reserved_name } ->
       begin
         List.iter (fun _ -> failf "extension entries unsupported") extension;
         List.iter (fun _ -> failf "nested types unsupported") nested_type;
         List.iter (fun _ -> failf "nested enums unsupported") enum_type;
         List.iter (fun _ -> failf "extension ranges unsupported") extension_range;
         List.iter (fun _ -> failf "oneof declarations unsupported") oneof_decl;
         List.iter (fun _ -> failf "reserved ranges unsupported") reserved_range;
         List.iter (fun _ -> failf "reserved names unsupported") reserved_name;

         iter_opt handle_options options;

         let msg = Context.message context name in
         List.iter (genfield context msg) field
       end
end

let generate1 _input_file
    { M.FileDescriptorProto.name; enum_type; message_type; service; package;
      dependency=_;        (* ? *)
      public_dependency=_; (* ? *)
      weak_dependency=_;   (* ? *)
      extension=_;         (* ? *)
      options=_;           (* ? *)
      source_code_info=_;  (* Locations *)
      syntax=_             (* protobuf syntax version (e.g. proto3) *) } :
  M.File.T.m Pb.msg =
  begin
    let context = Context.create ~package in
    begin match service with [] -> () | _ :: _ -> failf "Services not yet supported" end;
    List.iter (Enum.generate context) enum_type;
    let () = List.iter (Message.generate context) message_type in
    let name = map_opt
        (fun c -> Filename.chop_extension c ^ ".ml") name in
    M.File.mk ?name
      ~content:(Format.asprintf "%a" Context.write_ml context)
      ()
  end

let generate files proto_files =
  match List.combine files proto_files with
  | [(file, proto_file)] -> [generate1 file proto_file]
  | [] -> failf "error: exactly one filename (not zero) must be specified"
  | _::_::_ -> failf "error: exactly one filename (not more) must be specified"
  | exception Invalid_argument _ ->
    failf "Mismatched lengths: %d files, %d proto_files"
      (List.length files)
      (List.length proto_files)

let run msg =
  let { M.CodeGeneratorRequest.file_to_generate;
        proto_file; parameter=_; compiler_version=_ } =
    M.CodeGeneratorRequest.extract msg
  in
  match generate file_to_generate proto_file with
  | files ->
    M.CodeGeneratorResponse.mk ~file:files ()
  | exception (Error s) ->
    M.CodeGeneratorResponse.mk ~file:[] ~error:s ()


let main () =
  let stdin_buf = BatIO.(read_all stdin) in
  match Angstrom.parse_string (Pb.read M.CodeGeneratorRequest.T.t) stdin_buf with
    Result.Error e -> failf "Error parssing request: %s" e
  | Result.Ok msg -> print_string (Faraday.serialize_to_string
                                     (Pb.write (run msg)))

let () = main ()
