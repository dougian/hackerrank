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
let algebraic (a:seq<float>) (b:seq<float>) (x:float) =
    Seq.initInfinite (fun i -> (Seq.item i a) * x ** (Seq.item i b))


let ci (l:int) (step:float) i = 
    float l + step * i

let integral (l:int) (r:int) (step:float) (f:float->float) = 
    [float l .. step .. float r]
    |> List.map (fun i -> f (ci l step i))
    |> List.sum

let area (l:int) (r:int) f =
    integral l r 0.0001 f


(* The pascal triangle *)

let pascal_row n = 
    Seq.initInfinite (fun i -> (fact n) / ((fact i) * (fact (n-i))))
    |> Seq.take (n+1)

let pascal_triangle k = 
    Seq.initInfinite (fun i -> (pascal_row i))
    |> Seq.take k

(* The sierpinski triangle *)
let width k = 
    2 * k - 1

let repr size row i = 
    if i < size - row || i > size + row then
        "_"
    else 
        "1"

let sierpinski_row (size:int) row = 
    let row_repr = repr size row 
    [| 1..(width size) |]
    |> Array.map row_repr 

let triangle k = 
    let row = sierpinski_row k 
    [| 0..k-1 |]
    |> Array.map row 

let print_triangle (triangle : string [] []) = 
    triangle
    |> Array.map (fun i -> String.concat "" i)
    |> String.concat "\n"

type triangle_point = { x: int ; x2 : int ; y : int ; y2 : int}

let zeroize size (location : triangle_point) (triangle : string [] []) = 
    let start_row = location.y
    let end_row = location.y2
    let start_col = location.x
    let end_col = location.x2

    let factor row = 
        2 * (row - start_row)
    let repr size row i item= 
        if i < start_col || i > end_col then
            item
        elif i >= start_col + size - row + factor row && i  < end_col - size + row - factor row then
            "_"
        else 
            item
    let row_mapping row (x : string[]) = 
        let row_repr = repr size row
        if row >= start_row && row <= end_row then
            x 
            |> Array.mapi row_repr
        else
            x
    triangle
    |> Array.mapi row_mapping


let top_from_current (curr : triangle_point) = 
    [{ curr with y =  curr.y - (int curr.y / 2)}]

let bot_from_current (curr : triangle_point) = 
    let trans x = 
        (float x) / 2.0 |> ceil |> int
    [
        { curr with y = curr.y + (int curr.y / 2) ; x2 = curr.x2 - (trans curr.x2) };
        { curr with y = curr.y + (int curr.y / 2) ; x = curr.x + (int curr.x / 2)}
    ]

let rec fractal_places iter size = 
    match iter with
    | 0 -> [{ y = (int size/2) ; y2 = size; x = 0 ; x2 = (width size)}]
    | _ -> 
        let prev = fractal_places (iter - 1) size
        let bot = prev |> List.map bot_from_current |> List.concat
        let top = prev |> List.map top_from_current |> List.concat
        List.concat [prev ; top; bot;]
        
let create_triangle iter size = 
    let apply_changes =
        fractal_places iter size
        |> List.map ( fun location -> zeroize size location)
        |> List.reduce (>>)
    triangle size
    |> apply_changes

(* String Mingling *)
let mingle (a:string) (b:string) =
    let split_to_list (x:string) = x.ToCharArray() |> List.ofArray |> List.map string
    List.zip (split_to_list a) (split_to_list b)
    |> List.collect (fun (x,y) -> [x;y] ) 
    |> String.concat ""

(* Entrypoint to run the function needed every time *)

[<EntryPoint>]
let main argv = 
   let a = Console.ReadLine() |> int
   create_triangle a 32
   |> print_triangle
   |> printfn "%s"
   0