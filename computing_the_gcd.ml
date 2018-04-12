let parse str = 
    let j = String.index str ' ' in
    let u = int_of_string(String.sub str 0 j) in
    let v = int_of_string(String.sub str (j + 1) (String.length str - j - 1)) in
    (u,v)

let read_pair () =
    let now = read_line () in
    parse now

let rec gcd x y =
    if x < y then gcd y x
    else if y = 0 then x
    else gcd y (x mod y)

let () =
    let (x,y) = read_pair () in
    let res = gcd x y in
    let () = print_int res in
    print_newline ()
