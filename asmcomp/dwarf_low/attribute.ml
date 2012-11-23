(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                  Mark Shinwell, Jane Street Europe                  *)
(*                                                                     *)
(*  Copyright and licence information to be added.                     *)
(*                                                                     *)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)

open Std_internal

type t =
  | DW_AT_low_pc
  | DW_AT_high_pc
  | DW_AT_name
  | DW_AT_comp_dir
  | DW_AT_producer
  | DW_AT_stmt_list
  | DW_AT_external
  | DW_AT_location
  | DW_AT_type
  | DW_AT_encoding
  | DW_AT_byte_size

let encode = function
  | DW_AT_low_pc -> 0x11
  | DW_AT_high_pc -> 0x12
  | DW_AT_name -> 0x03
  | DW_AT_comp_dir -> 0x1b
  | DW_AT_producer -> 0x25
  | DW_AT_stmt_list -> 0x10
  | DW_AT_external -> 0x3f
  | DW_AT_location -> 0x02
  | DW_AT_type -> 0x49
  | DW_AT_encoding -> 0x3e
  | DW_AT_byte_size -> 0x0b

let form = function
  | DW_AT_low_pc -> Form.addr
  | DW_AT_high_pc -> Form.addr
  | DW_AT_name -> Form.string
  | DW_AT_comp_dir -> Form.string
  | DW_AT_producer -> Form.string
  | DW_AT_stmt_list -> Form.data4
  | DW_AT_external -> Form.flag
  | DW_AT_location -> Form.data8
  | DW_AT_type -> Form.ref_addr
  | DW_AT_encoding -> Form.data1
  | DW_AT_byte_size -> Form.data1

let low_pc = DW_AT_low_pc
let high_pc = DW_AT_high_pc
let producer = DW_AT_producer
let name = DW_AT_name
let comp_dir = DW_AT_comp_dir
let stmt_list = DW_AT_stmt_list
let extern'l = DW_AT_external
let location = DW_AT_location
let typ' = DW_AT_type
let encoding = DW_AT_encoding
let byte_size = DW_AT_byte_size

let size t =
  Value.size (Value.as_uleb128 (encode t)) + Form.size (form t)

let emit t ~emitter =
  Value.emit (Value.as_uleb128 (encode t)) ~emitter;
  Form.emit (form t) ~emitter
