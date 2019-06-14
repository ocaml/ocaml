(* TEST *)

(*

A test file for Format.set_max_indent_offset.

*)

open Format;;

let set_margin n =
  Format.set_margin n;
  Format.set_max_indent (n - 1)
;;

let test_max_indent_offset n =
  print_string "max_indent_offset = ";
  print_int n;
  print_newline ();
  set_margin 20;
  set_max_indent_offset n;
  print_break 0 2;
  open_hvbox 0;
  begin
    print_string "foooooooooooooooooooo";
    print_space ();
    print_string "foooooooooooooooooooo";
    print_break 0 4;
    open_hvbox 0;
    begin
      print_string "foooooooooooooooooooo";
      print_space ();
      print_string "foooooooooooooooooooo";
      print_break 0 6;
      open_hvbox 0;
      begin
        print_string "foooooooooooooooooooo";
        print_space ();
        print_string "foooooooooooooooooooo";
        print_space ();
        print_string "foooooooooooooooooooo";
      end;
      close_box ()
    end;
    close_box ()
  end;
  close_box ();
  print_newline ()
;;

test_max_indent_offset 4;;
test_max_indent_offset 3;;
test_max_indent_offset 2;;
test_max_indent_offset 1;;
