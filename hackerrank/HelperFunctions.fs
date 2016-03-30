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

    let rec my_filter pred arr = 
        match arr with
        | [] -> []
        | head :: tail ->
            if pred(head) then
                List.append [head] (my_filter pred tail)
            else
                my_filter pred tail
