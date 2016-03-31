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

    let rec custom_length list = 
        match list with 
        | [] -> 0
        | _ :: tail -> 
            1 + custom_length tail