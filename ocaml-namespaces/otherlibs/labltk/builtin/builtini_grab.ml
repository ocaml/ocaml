let cCAMLtoTKgrabGlobal x =
  if x then TkToken "-global" else TkTokenList []
