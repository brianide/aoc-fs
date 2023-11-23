module Year2020.Day5

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Util.Patterns

let parse path =
    let toBit = function
    | 'F' | 'L' -> 0
    | 'B' | 'R' -> 1
    | c -> failwithf "Invalid input: %A" c

    let parseLine line = Seq.fold (fun acc n -> (acc <<< 1) ||| (toBit n)) 0 line
    
    File.ReadAllLines path
    |> Array.toList
    |> List.map parseLine

let solveSilver input =
    List.max input
    |> sprintf "%A"
    
let solveGold input =
    List.sort input
    |> List.pairwise
    |> List.filter (fun (a, b) -> b - a = 2)
    |> List.exactlyOne
    |> fst
    |> (+) 1
    |> sprintf "%A"

[<Solution("2020", "5", "Binary Boarding")>]
let Solver = chainFileHandler parse solveSilver solveGold