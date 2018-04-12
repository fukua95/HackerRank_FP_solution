let solve (s : string) (t : string) : int = 
    let len = min (String.length s) (String.length t) in
    let rec f p =
        if p = len then p
        else if s.[p] = t.[p] then f (p + 1)
        else p
    in
    f 0

let print (l : int) (r : int) (str : string) : unit = 
    let rec f p =
        if p = r then print_newline ()
        else let () = print_char str.[p] in f (p + 1)
    in
    let len = r - l in
    let () = print_int len in
    if len = 0 then print_newline ()
    else let () = print_char ' ' in f l

let () =
    let s = read_line () in
    let t = read_line () in
    let pos = solve s t in
    let () = print 0 pos s in
    let () = print pos (String.length s) s in
    print pos (String.length t) t
