(* $Id$ *)

open Widget

type edit_window =
  { mutable name: string;
    tw: text widget;
    frame: frame widget;
    modified: Textvariable.textVariable;
    mutable shell: (string * Shell.shell) option;
    mutable structure: Typedtree.structure;
    mutable signature: Types.signature;
    mutable psignature: Parsetree.signature;
    number: string }
