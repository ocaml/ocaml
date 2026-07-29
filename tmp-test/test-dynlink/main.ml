let () = Dynlink.loadfile (if Dynlink.is_native then "loaded.cmxs" else "loaded.cmo")
