#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "pb"
    ~licenses:[Pkg.std_file "LICENSE"]
  @@ fun c ->
    Ok [ Pkg.mllib ~api:["Pb"] "lib/pb.mllib";
         Pkg.doc "README.md";
         Pkg.test ~dir:"lib_test" "lib_test/test"; ]
           
