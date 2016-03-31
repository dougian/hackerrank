// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open HelperFunctions.General
open System
open System.Linq 

(* problem with the list repeat *)

let repeater repeat_times k =
    Enumerable.Repeat (k,repeat_times)

let list_replication argv = 
    let repeat_times = 
        Console.ReadLine()
        |> Int32.Parse
    let repeater_by_input = repeater repeat_times
    let list_input = 
        read_lines
        |> Seq.map Int32.Parse
        |> Seq.map repeater_by_input
        |> Seq.concat
    list_input

(* array filter problem *)
let array_filter argv =
    let delimiter = 
        Console.ReadLine()
        |> Int32.Parse
    let data = 
        read_lines
        |> Seq.map Int32.Parse
        |> List.ofSeq
        |> custom_filter (fun x -> x > delimiter)
    data

(* filter positions *)
let filter_positions argv =
    let data = 
        read_lines
        |> Seq.map Int32.Parse
        |> Seq.mapi (fun i k -> (i,k))
        |> Seq.filter (fun t -> (fst t)%2 <> 0)
        |> Seq.map snd
    data

(* return an array of N items *)
let f n = 
    Seq.initInfinite (fun i -> i + 1)
    |> Seq.take n
    |> List.ofSeq

(* Reverse a list without using the build in reverse *)
let reverse argv = 
    let data = 
        read_lines
        |> Seq.map Int32.Parse
        |> List.ofSeq
        |> reverse_list
    data

(* Sum of Odd Elements *)
let sum_of_odd argv = 
    argv
    |> Seq.filter (fun x -> x % 2 <> 0)
    |> Seq.sum


(* Count the number of elements in an array without using count, size or length operators *)

(* Entrypoint to run the function needed every time *)

[<EntryPoint>]
let main argv = 
    let input_data = 
        read_lines
        |> Seq.map Int32.Parse
        |> List.ofSeq
        |> custom_length
    printfn "%d" input_data

    input_data