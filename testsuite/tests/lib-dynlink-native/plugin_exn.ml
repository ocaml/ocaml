exception Plugin_exception

let () =
  Api.reg_mod "Plugin_exn";
  let handle_exception = function
  | Plugin_exception ->
    begin
      print_endline "Caught Plugin_exception";
      Api.restore_exn_handler ()
    end
  | exn ->
    begin
      Api.restore_exn_handler ();
      raise exn
    end
  in
  let cb () =
    Api.set_exn_handler handle_exception;
    print_endline "Raising Plugin_exception";
    raise Plugin_exception
  in
  Api.add_cb cb;
  print_endline "Plugin_exn loaded"
