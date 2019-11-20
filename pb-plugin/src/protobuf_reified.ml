(*
 * Copyright (c) 2017 Jeremy Yallop <yallop@docker.com>.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let failf fmt = Printf.kprintf failwith fmt

module type GEN =
sig
  type message
  val message : string -> message

  type enum
  val enum : string -> enum
  val constant : string -> enum -> int32 -> unit

  type field
  val enum_field : enum -> field
  val message_field : message -> field
  val bool : field
  val int32 : field
  val sint32 : field
  val sfixed32 : field
  val int64 : field
  val sint64 : field
  val sfixed64 : field
  val uint32 : field
  val fixed32 : field
  val uint64 : field
  val fixed64 : field
  val double : field
  val float : field
  val string : field
  val bytes : field

  val required : string -> message -> field -> int32 -> unit
  val optional : ?default:string -> string -> message -> field -> int32 -> unit
  val repeated : string -> message -> field -> int32 -> unit
end

module Generate : sig
  include GEN
  val write : Format.formatter -> message list -> enum list -> unit
end =
struct
  [@@@ocaml.warning "-34"]
  let ocaml_keywords = [
    "and"; "as"; "assert"; "asr"; "begin"; "class"; "constraint";
    "do"; "done"; "downto"; "effect"; "else"; "end"; "exception";
    "external"; "false"; "for"; "fun"; "function"; "functor"; "if";
    "implicit"; "in"; "include"; "inherit"; "initializer"; "land";
    "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "macro"; "match";
    "method"; "mod"; "module"; "mutable"; "new"; "object"; "of";
    "open"; "or"; "private"; "rec"; "sig"; "struct"; "then"; "to";
    "true"; "try"; "type"; "val"; "virtual"; "when"; "with"; "while"
  ]

  let safe_id id = if List.mem id ocaml_keywords then id ^ "_" else id

  type message = {
    message_name: string;
    mutable fields: message_field list;
  }
  and enum = {
    enum_name: string;
    mutable constants: (string * int32) list;
  }
  and field =
      Message of message
    | Enum of enum
    | Basic of basic
  and message_field = {
    field_name: string;
    field_number: int32;
    field_type: field;
    field_class: [`required | `optional of string option | `repeated]
  }
  and method_ = {
    method_name: string;
    input_type: string;
    output_type: string;
  }
  and basic = {
    basic_type: string;
    basic_constructor: string;
  }
  let basic typ c : field = Basic { basic_type=typ; basic_constructor=c }
  let message name : message = { message_name=name; fields = [] }
  and enum name : enum = { enum_name=name; constants = [] }
  and constant name enum value = enum.constants <- (name, value) :: enum.constants
  and enum_field e = Enum e
  and message_field m = Message m
  and bool     = basic "bool" "bool"
  and int32    = basic "int32" "int32"
  and sint32   = basic "int32" "sint32"
  and sfixed32 = basic "int32" "sfixed32"
  and int64    = basic "int64" "int64"
  and sint64   = basic "int64" "sint64"
  and sfixed64 = basic "int64" "sfixed64"
  and uint32   = basic "uint32" "uint32"
  and fixed32  = basic "uint32" "fixed32"
  and uint64   = basic "uint64" "uint64"
  and fixed64  = basic "uint64" "fixed64"
  and double   = basic "double" "double"
  and float    = basic "float" "float"
  and string   = basic "string" "string"
  and bytes    = basic "Bytes.t" "bytes"
  and required field_name msg field_type field_number =
    msg.fields <- { field_name; field_type; field_number; field_class = `required } :: msg.fields
  and optional ?default field_name msg field_type field_number =
    msg.fields <- { field_name; field_type; field_number; field_class = `optional default } :: msg.fields
  and repeated field_name msg field_type field_number =
    msg.fields <- { field_name; field_type; field_number; field_class = `repeated } :: msg.fields

  let default_value typ fmt s =
    Format.fprintf fmt "%s"
    (match typ, s with
     | Message _m, _ -> failf "Default values of message type are not supported"
     | Enum e, n when List.mem_assoc n e.constants ->
        let module_name = String.capitalize_ascii e.enum_name in
        let constant_name = String.lowercase_ascii n in
        Printf.sprintf "%s__constants.%s" module_name constant_name
     | Enum e, n -> failf "%s is not a member of enum %s\n" n e.enum_name
     | Basic {basic_type="bool";_}, "false" -> "false"
     | Basic {basic_type="bool";_}, "true" -> "true"
     | Basic {basic_type=t;_}, v ->
       Printf.sprintf "(assert false) (* %s of type %s *)" v t)

  let write_class field_type fmt = function
    | `required -> Format.pp_print_string fmt "required"
    | `optional None -> Format.pp_print_string fmt "optional"
    | `optional (Some d) -> Format.fprintf fmt "optional@ ~default:%a" (default_value field_type) d
    | `repeated -> Format.pp_print_string fmt "repeated"

  let write_field ~qualify fmt = function
    | Message {message_name;_} when qualify ->
      Format.fprintf fmt "Types_.s_%s" (String.capitalize_ascii message_name)
    | Message {message_name;_} ->
      Format.fprintf fmt "s_%s" (String.capitalize_ascii message_name)
    | Enum {enum_name;_} when qualify -> 
      Format.fprintf fmt "@[Types_.%s.e@ Pb.enum@]" (String.capitalize_ascii enum_name)
    | Enum {enum_name;_} -> 
      Format.fprintf fmt "@[%s.e@ Pb.enum@]" (String.capitalize_ascii enum_name)
    | Basic s ->
      Format.fprintf fmt "%s" s.basic_type

  let max_length = List.fold_left (fun l s -> max (String.length s) l) 0

  let write_record_field_type ~qualify fmt (cls, typ) = match cls with
    | `required -> (write_field ~qualify) fmt typ
    | `optional None -> Format.fprintf fmt "@[%a@ option@]" (write_field ~qualify) typ
    | `optional (Some _) -> Format.fprintf fmt "@[%a@]" (write_field ~qualify) typ
    | `repeated -> Format.fprintf fmt "@[%a@ list@]" (write_field ~qualify) typ

  let write_record_rhs ~qualify fmt fields =
    let pr f = Format.fprintf fmt f in
    ListLabels.iter fields ~f:begin fun {field_name; field_type; field_class; _} ->
       pr "@ @ @[%s:@ %a@];@\n"
          (safe_id field_name) (write_record_field_type ~qualify) (field_class, field_type);
    end

  let write_message fmt { message_name; fields } =
    let pr f = Format.fprintf fmt f in
    let module_name = String.capitalize_ascii message_name in
    pr "@[module@ %s@ =@ struct@\n" module_name;
    pr "@ @[include Types_.Fields__%s@]@\n@\n" module_name;
    pr "@ @[type s = Types_.s_%s = {@\n%a}@]@\n" module_name (write_record_rhs ~qualify:true) fields;
    pr "@\n";

(*
  Sample target code:

  type s = {
    ctype: CType.T.e enum;
    packed: bool option;
    jstype: JSType.T.e enum;
    lazy_: bool;
    deprecated: bool;
    weak: bool;
    uninterpreted_option: UninterpretedOption.s list;
  }

  let extract msg = {
    ctype = from_opt ~default:CType.string (getf msg ctype);
    packed = getf msg packed;
    jstype = from_opt ~default:JSType.normal (getf msg jstype);
    lazy_ = from_opt ~default:false (getf msg lazy_);
    deprecated = from_opt ~default:false (getf msg deprecated);
    weak = from_opt ~default:false (getf msg weak);
    uninterpreted_option = List.map UninterpretedOption.extract (getf msg uninterpreted_option);
  }
*)
    pr "@ @[let@ extract@ =@ Types_.extract_%s@]@\n@\n" module_name;

(*
  Sample target code:

  let mk ?ctype:x0 ?packed:x1 ?jstype:x2 ?lazy_:x3 ?deprecated:x4 ?weak:x5 ~uninterpreted_option:x6 () =
    let msg = create T.t in
    setf msg ctype x0;
    setf msg packed x1;
    setf msg jstype x2;
    setf msg lazy_ x3;
    setf msg deprecated x4;
    setf msg weak x5;
    setf msg uninterpreted_option x6;
    msg
*)
    let arg cls i name =
      match cls with
      | `required | `repeated ->  
        pr "@[~%s:_local%d@]@ " (safe_id name) i
      | `optional _ ->
        pr "@[?%s:_local%d@]@ " (safe_id name) i
(* TODO: we could use default arguments here: *)
(*
      | `optional None ->
        pr "@[?%s:_local%d@]@ " (safe_id name) i
      | `optional (Some default) ->
        pr "@[?%s:(_local%d=%s)@]@ " (safe_id name) i default
 *)
    in

    pr "@ @[<h 1>let@ @[mk@ ";
    ListLabels.iteri fields ~f:begin
      fun i {field_name; field_class; _} ->
        arg field_class i field_name
    end;
    pr "@ ()@ =@]@\n";

    pr "@ let@ _msg@ =@ @[Pb.create@ T.t@]@ in@\n";

    ListLabels.iteri fields ~f:begin
      fun i {field_name; field_number=_; field_type=_; field_class=_} ->
        pr "@ @[Pb.setf@ _msg@ %s@ _local%d@];@\n" (safe_id field_name) i
    end;
    pr "@ _msg@]@\n";


    pr "end@]@\n@\n"

(* Mutual recursion: what can depend on what?

     type declarations 'module T = ...' do not depend on anything

     field declarations 'let x = T.required ...' may depend on any type declaration and on enum constants

     s declarations may depend on any type declaration

     extract declarations may depend on
         any type declaration 
         and any extract declaration
         and any s declaration
         and any field declaration

     mk declarations may depend on
         the type declaration 
         and the field declaration for the type
 *)

  let write_forward_declarations fmt messages enums =
    let pr f = Format.fprintf fmt f in
    (* Pb.message bindings *)
    pr "module@ Types_@ =@ struct@\n@[<v 2>";
    ListLabels.iter messages ~f:begin fun { message_name; _ } ->
      let module_name = String.capitalize_ascii message_name in
      pr "@ @[module@ %s@ =@ @[<hov 2>(val@ Pb.message@ %S)@]@]" module_name module_name;
    end;
    ListLabels.iter enums ~f:begin fun { enum_name; _ } ->
      let module_name = String.capitalize_ascii enum_name in
      pr "@ @[module@ %s@ =@ @[<hov 2>(val@ Pb.enum@ %S)@]@]" module_name module_name;
    end;
    ListLabels.iter enums ~f:begin fun { enum_name; constants } ->
      let module_name = String.capitalize_ascii enum_name in
      pr "@ @[module@ %s__constants@ =@ @[struct@\n" module_name;
      ListLabels.iter (List.rev constants) ~f:begin fun (name, value) ->
        pr "@[@ let@ %s@ = %s.constant@ %S@ %ldl@]@\n"
           (String.lowercase_ascii name) module_name name value;
      end;
      pr "end@]@]@\n";
    end;

    (* name bindings *)
    let write_field_type fmt typ =
      match typ with
        Basic b -> Format.fprintf fmt "Pb.%s" b.basic_constructor
      | Message m -> Format.fprintf fmt "(Pb.msg@ %s.t)"
                       (String.capitalize_ascii m.message_name)
      | Enum e -> Format.fprintf fmt "%s.t"
                    (String.capitalize_ascii e.enum_name)
    in

    ListLabels.iter messages ~f:begin fun { message_name; fields; _ } ->
      let module_name = String.capitalize_ascii message_name in
      pr "@ @[module@ Fields__%s@ =@ struct@\n" message_name;
      pr "@ @[module@ T@ =@ %s@]@\n@\n" module_name;
      let max_field_name_length = max_length
        (List.map (fun {field_name;_} -> field_name) fields) in
      ListLabels.iter (List.rev fields) ~f:begin
        fun {field_name; field_number; field_type; field_class} ->
          pr "@ @[<hov 2>let@ %s%*s@ @[T.%a@ %a@ %S@ %ld@]@]@\n"
            (safe_id field_name) (2 + (max_field_name_length - String.length field_name)) "="
            (write_class field_type) field_class
            write_field_type field_type
            field_name
            field_number
          ;
      end;
      pr "@\nend@\n@]@\n";
    end;


    (* type s bindings *)
    ListLabels.iteri messages ~f:begin fun i { message_name; fields; _ } ->
      let module_name = String.capitalize_ascii message_name in
      pr "@ @[%s@ s_%s@ =@ {@\n" (if i = 0 then "type" else "and") module_name;
      write_record_rhs ~qualify:false fmt fields;
      pr "}@]";
    end;

    (* extract bindings *)

    let field_expr cls typ name =
      match cls, typ with
      | `required, Message m ->
        pr "@[extract_%s@ (Pb.getf@ _msg@ %s)@]"
          (String.capitalize_ascii m.message_name)
          (safe_id name)
      | `required, _ ->
        pr "@[Pb.getf@ _msg@ %s@]" (safe_id name)
      | `repeated, Message m ->
        pr "@[List.map@ extract_%s@ @[(Pb.getf@ _msg@ %s)@]@]"
           (String.capitalize_ascii m.message_name) name 
      | `repeated, Enum _ ->
        (* pr "@[List.map@ %s.extract@ @[(Pb.getf@ _msg@ %s)@]@]" e.enum_name name  *)
         pr "@[(Pb.getf@ _msg@ %s)@]" name
      | `repeated, Basic _ ->
        pr "@[Pb.getf@ _msg@ %s@]" name
      | `optional None, Message m ->
         pr "@[opt_map extract_%s @[(Pb.getf@ _msg@ %s)@]@]"
            (String.capitalize_ascii m.message_name) name
      | `optional None, _ ->
         pr "@[Pb.getf@ _msg@ %s@]" name
      | `optional (Some default), _ ->
         pr "@[from_opt@ ~default:%a@ (Pb.getf@ _msg@ %s)@]" (default_value typ) default name
    in


    ListLabels.iteri messages ~f:begin fun i { message_name; fields; _ } ->
      let module_name = String.capitalize_ascii message_name in
      pr "@ @[%s@ extract_%s@ _msg@ =@ Fields__%s.(({@\n"
         (if i = 0 then "let rec" else "and")
         module_name
         module_name;
      ListLabels.iter (List.rev fields) ~f:begin
        fun {field_name; field_type; field_class; _} ->
          pr "@ @ @[%s@ =@ " (safe_id field_name);
          field_expr field_class field_type (safe_id field_name);
          pr "@];@\n";
      end;
      pr "}@ : s_%s))@]" module_name;
    end;

    pr "@]@\nend@\n@\n"
    
  let write_prologue fmt =
    let pr f = Format.fprintf fmt f in
    begin
      pr "@[(* Generated by ocaml-pb-plugin *)@]@\n@\n";
      pr "[@@@@@@ocaml.warning \"-30\"] (* record labels reused *)@\n@\n";
      pr "open Unsigned@\n";
      pr "module type TRANSPORT = sig end@\n";
      pr "type double = float@\n";
      pr "let from_opt ~default = function Some v -> v | None -> default@\n";
      pr "let opt_map f = function Some v -> Some (f v) | None -> None@\n";
      pr "let rassoc y l = fst (List.find (fun (_,y') -> y = y') l)@\n";
      pr "@\n";
    end

  let write_enum fmt { enum_name; constants } =
    let pr f = Format.fprintf fmt f in
    let module_name = String.capitalize_ascii enum_name in
    begin
      pr "module@ %s@ =@ @[<hov 2>struct@\n" module_name;
      pr "@[module T = struct@\n@[<h 2>@;@;";

      pr "@[type@ e@ =@ Types_.%s.e@]@\n" module_name;

      pr "@[<hov 2>let@ __map@ =@ [";
      ListLabels.iter (List.rev constants) ~f:begin fun (name, v) ->
        pr "@\n@ @[%ldl,@ `%s@];" v name;
      end;
      pr "@]@\n]@\n";
      pr "@[let of_value x = List.assoc x __map@]@\n";
      pr "@[let to_value x = rassoc x __map@]";
      pr "@]@]@\nend@\n";
      pr "@[include@ Types_.%s__constants@]@\n" module_name;
      pr "@]@\nend@\n@\n";
    end

  let write fmt messages enums =
    write_prologue fmt;
    write_forward_declarations fmt messages enums;
    List.iter (write_message fmt) messages;
    List.iter (write_enum fmt) enums
end
