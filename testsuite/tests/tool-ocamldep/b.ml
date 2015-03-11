let x = Lib1.map String.length []

module Local = struct
  module Lib2 = struct
    let fold _ = assert false
  end
  let _ = Lib2.fold (fun _ -> assert false)
end
