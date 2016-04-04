namespace HelperFunctions

open System

module General = 
    let read_lines = 
        Seq.initInfinite (fun _ -> Console.ReadLine())
        |>  Seq.takeWhile (fun x -> x<> null && x <> "")

    let printSeq arr = 
        let smart_print = printfn "%A"
        arr |>
            Seq.map smart_print

    let rec custom_filter pred list = 
        match list with
        | [] -> []
        | head :: tail ->
            if pred(head) then
                List.append [head] (custom_filter pred tail)
            else
                custom_filter pred tail
     
    let rec reverse_list list =
        match list with
        | [] -> []
        | head :: tail ->
           List.append (reverse_list tail) [head]

(* Count the number of elements in an array without using count, size or length operators *)
    let rec custom_length list = 
        match list with 
        | [] -> 0
        | _ :: tail -> 
            1 + custom_length tail

    let rec fact x =
        if x < 1 then 1
        else x * fact (x - 1)


    let rec gcd x y = 
        if x = y then
            x
        elif x > y then
            gcd (x-y) y
        else 
            gcd y x

    let fibonacci = 
        Seq.unfold (fun (fst,snd) -> Some(fst + snd, (snd, fst + snd))) (0,1)