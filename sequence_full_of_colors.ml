let solve (str : string) : bool = 
    let check x y = 
        let cur = x - y in
        if -1 <= cur && cur <= 1 then true else false
    in
    let len = String.length str in
    let rec f pos r g y b =
        if not (check r g) || not (check y b) then (false,0,0,0,0) 
        else if pos = len then (true,r,g,y,b)
        else if str.[pos] = 'R' then f (pos + 1) (r + 1) g y b
        else if str.[pos] = 'G' then f (pos + 1) r (g + 1) y b
        else if str.[pos] = 'Y' then f (pos + 1) r g (y + 1) b
        else f (pos + 1) r g y (b + 1)
    in
    let (can,r,g,y,b) = f 0 0 0 0 0 in
    if not can then false 
    else if r != g || y != b then false
    else true

let () =
    let t = read_int () in
    for i = 1 to t do 
        let str = read_line () in
        let () = if solve str then print_string "True" else print_string "False" in
        print_newline ()
    done
