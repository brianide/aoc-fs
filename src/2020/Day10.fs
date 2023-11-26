module Year2020.Day10

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions

let parse = File.ReadAllLines >> Seq.map int64 >> Seq.toList

let solveSilver input =
    seq { [0L; (List.max input) + 3L]; input }
    |> List.concat
    |> List.sort
    |> List.pairwise
    |> List.map (fun (a, b) -> b - a)
    |> List.groupBy id
    |> List.map (snd >> List.length)
    |> List.reduce (*)
    |> sprintf "%i"

let rec tribonacci = function
| 0 -> 0
| 1 -> 0
| 2 -> 1
| n -> tribonacci (n - 1) + tribonacci (n - 2) + tribonacci (n - 3)

let solveGold input =
    seq { [0L; (List.max input) + 3L]; input }
    |> List.concat
    |> List.sort
    |> List.pairwise
    |> List.map (fun (a, b) -> b - a)
    |> Seq.partitionBy id
    |> Seq.filter (fun arr -> List.head arr = 1L)
    |> Seq.map List.length
    |> Seq.filter (fun l -> l >= 2)
    |> Seq.map (fun n -> tribonacci (n + 2) |> uint64)
    |> Seq.reduce (*)
    |> sprintf "%i"

[<Solution("2020", "10", "Adapter Array")>]
let Solver = chainFileHandler parse solveSilver solveGold