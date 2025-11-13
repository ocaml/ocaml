let calculate x y =
  let sum = x + y in
  let product = x * y in
  Printf.printf "sum=%d product=%d\n" sum product;
  product

let () =
  let result = calculate 10 20 in
  Printf.printf "result=%d\n" result
