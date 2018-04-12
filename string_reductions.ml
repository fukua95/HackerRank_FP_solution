let solve (str : string) : char list = 
    let len = String.length str in
    let rec in_rem (ch : char) (rem : char list) : bool = 
        match rem with
        | [] -> false
        | hd :: tl -> if ch = hd then true else in_rem ch tl
    in
    let rec f acc rem p = 
        if p = len then acc
        else if (in_rem str.[p] rem) then f acc rem (p + 1)
        else f (str.[p] :: acc) (str.[p] :: rem) (p + 1)
    in 
    List.rev (f [] [] 0)

let () =
    let str = read_line () in
    let rec print s =
        match s with
        | [] -> print_newline ()
        | hd :: tl -> print_char hd; print tl
    in
    print (solve str)
