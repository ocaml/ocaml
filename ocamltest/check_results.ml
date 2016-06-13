let check_output output_variable reference_variable ppf env =
  let output_filename = Environments.safe_lookup output_variable env in
  let reference_filename = Environments.safe_lookup reference_variable env in
  if Sys.file_exists reference_filename then
  begin (* Compare the two files *)
    let cmdline = "cmp -q " ^ output_filename ^ " " ^ reference_filename in
    match run_command ~stdout_variable:"" ~stderr_variable:"" ppf env cmd with
  end else (* Make sure output file is empty *)
  end
