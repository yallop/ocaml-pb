# pb, a library for describing Protobuf messages

`pb` is an OCaml library for describing and serializing [Protocol buffers (Protobuf)][protobuf] messages.

Message descriptions can be written by hand, or generated from `.proto` files using the [`pb-plugin`][pb-plugin] protoc compiler plugin.

## Describing messages

Protocol buffers provide both a file format for describing messages and a serialization (wire) format.  The `pb` library supports only the wire format, but there is a straightforward mapping from the file format into `pb` code.  Here is a description of a [Protobuf message][protobuf-message] with two fields, a `number` field with the [tag][protobuf-tag] `1`, and a `PhoneType` field with the tag `2` and default value `HOME`:


```protobuf
message PhoneNumber {
  required string number = 1;
  optional PhoneType type = 2 [default = HOME];
}
```

And here is the equivalent `pb` code that defines the `PhoneNumber` message: 

```ocaml
module PhoneNumber = (val message "PhoneNumber")
let number = PhoneNumber.required string "number" 1
let type_  = PhoneNumber.optional "type" 2 ~default:home
```

The `type` field of the `PhoneNumber` message has the type `PhoneType`, an [enumeration value][protobuf-enum].  Here is a description of the `PhoneType` enumeration with its three values:

```protobuf
enum PhoneType {
  MOBILE = 0;
  HOME = 1;
  WORK = 2;
}
```

And here is the equivalent `pb` code that defines the `PhoneType` enumeration: 


```ocaml
module PhoneType = (val enum "PhoneType")
let mobile = PhoneType.constant "MOBILE" 0l
let home   = PhoneType.constant "HOME" 1l
let work   = PhoneType.constant "WORK" 2l
```

## Serializing and deserializing

Messages described by `pb` can be serialized using the [Faraday][faraday] serialization library.  The following code creates a `PhoneNumber` message, assignes values to its two fields, and writes it to a Faraday serializer:

```ocaml
let pn = create PhoneNumber.t in
  setf pn number ("+1-541-754-3010");
  setf pn type_ work;
  write pn
```

Messages can be deserialized (parsed) using the [Angstrom][angstrom] parser-combinator library.  The following code reads a message using Angstrom and retrieves the values of its fields:


```ocaml
let pn = match Angstrom.parse_only (read PhoneNumber.t) (`String s)) with
         | Ok m -> m
         | Error s -> failwith s
  in (getf pn number, getf pn type_)
```

## Pretty-printing

Messages can also be pretty-printed:

```ocaml
pp_msg PhoneNumber.t Format.std_formatter pn
```

***

[![Travis build Status](https://travis-ci.org/yallop/ocaml-pb.svg?branch=master)](https://travis-ci.org/yallop/ocaml-pb) 

[protobuf]: https://developers.google.com/protocol-buffers/
[angstrom]: https://github.com/inhabitedtype/angstrom
[faraday]: https://github.com/inhabitedtype/faraday
[protobuf-enum]: https://developers.google.com/protocol-buffers/docs/proto#enum
[protobuf-message]: https://developers.google.com/protocol-buffers/docs/proto#simple
[protobuf-tag]: https://developers.google.com/protocol-buffers/docs/proto#assigning-tags
[protobuf-scalar]: https://developers.google.com/protocol-buffers/docs/proto#scalar
[pb-plugin]: https://github.com/yallop/ocaml-pb-plugin
