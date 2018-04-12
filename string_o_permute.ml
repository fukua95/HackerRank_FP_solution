let solve (str : string) : char list = 
    let len = String.length str in
    let rec f acc i =
        if i > len - 2 then acc
        else f (str.[i] :: str.[i + 1] :: acc) (i + 2)
    in List.rev (f [] 0)

let rec print_list (lst : char list) =
    match lst with
    | [] -> print_newline ()
    | hd :: tl -> print_char hd; print_list tl

let () = 
    let t = read_int () in
    for i = 1 to t do
        let str = read_line () in
        print_list (solve str)
    done
