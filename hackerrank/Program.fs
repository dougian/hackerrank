﻿// Learn more about F# at http://fsharp.net
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
let arr_of n = 
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
let sum_of_odd list = 
    list
    |> Seq.filter (fun x -> x % 2 <> 0)
    |> Seq.sum


(* Update list to absolute values *)
let update_list list = 
    list
    |> Seq.map abs

(* Evaluate the  e^x without using local variables *)

let exponential (x:float) = 
    Seq.initInfinite (fun i ->(x ** (float i)) / float (fact i))
    |> Seq.take 10
    |> Seq.sum


(* Compute the area under an algebraic expression by the limit definition of a definite ingregral *)
let algebraic (a:seq<'a>) (b:seq<'a>) (x:float) =
    Seq.initInfinite (fun i -> (Seq.item i a) * x ** (Seq.item i b))


let ci (l:int) (step:float) i = 
    float l + step * i

let integral (l:int) (r:int) (step:float) (f:float->float) = 
    [float l .. step .. float r]
    |> List.map (fun i -> f (ci l step i))
    |> List.sum

let area (l:int) (r:int) f =
    integral l r 0.0001 f

(* Entrypoint to run the function needed every time *)

[<EntryPoint>]
let main argv = 
   let a = Console.ReadLine()

   fibonacci
   |> Seq.item (int a)
   |> printfn "%A"
   0