let rec power (x : int) (y : int)  : int = 
    if y = 0 then 1
    else let cur = x * (power x (y - 1)) in if cur > 1000 then 1001 else cur

let solve (x : int) (k : int) : int =
    let sum = Array.create (x + 1) 0 in
    sum.(0) <- 1;
    for i = 1 to 31 do
        let cur = power i k in
        for j = x downto 0 do
            if (sum.(j) > 0 && j + cur <= x) then sum.(j+cur) <- sum.(j+cur) + sum.(j);
        done
    done;
    sum.(x)

let () =
    let x = read_int () in
    let k = read_int () in
    let () = print_int (solve x k) in
    print_newline ()
