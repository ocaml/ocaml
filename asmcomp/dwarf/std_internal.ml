module List = struct
  include ListLabels
  let fold = fold_left
end

let sprintf = Printf.sprintf
