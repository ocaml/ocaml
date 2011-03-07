let i = "toto" in do { (let i = 42 in print_int i); print_string i };
let i = "toto" in do { print_string i; let i = 42 in print_int i; print_int i };
let i = "toto" in do {
  (let i = 42 in print_int i);
  let i = i ^ i;
  let i = i ^ i;
  print_string i;
  print_string i;
  let i = i ^ i;
  print_string i };
