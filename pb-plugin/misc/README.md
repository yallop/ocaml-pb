Various `.proto` files:

* [`messages.proto`](messages.proto):
  The protobuf messages that define the interface between the `protoc`
  compiler and plugins.

* [`messages_unnested.proto`](messages_unnested.proto):
  An variant of `messages.proto` without nested messages, which are
  not currently supported by this plugin.
