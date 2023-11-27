module Year2020.Day13

open System.IO
open System.Text.RegularExpressions
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Patterns

#nowarn "25"

let solveSilver path =
    let start :: intervals =
        File.ReadAllText path
        |> Regex(@"\d+").Matches
        |> Seq.map (_.Value >> int)
        |> Seq.toList

    intervals
    |> List.map (fun n -> (n, (start / n + 1) * n - start))
    |> List.minBy snd
    |> fun (a, b) -> a * b
    |> sprintf "%i"

let solveGold path =
    let buses =
        File.ReadAllLines path
        |> Seq.item 1
        |> _.Split(',')
        |> Seq.mapi (fun i s -> match s with TryParse int n -> Some (uint64 i, uint64 n) | _ -> None)
        |> Seq.choose id
        |> Seq.toList
    
    let folder (acco, accb) (o, b) =
        Seq.initInfinite ((+) 1 >> uint64)
        |> Seq.find (fun i -> (acco + accb * i - o) % b = uint64 0)
        |> fun i -> (acco + accb * i, accb * b)

    List.fold folder (uint64 0, uint64 1) buses
    |> fun (o, b) -> b - o
    |> sprintf "%i"

[<Solution("2020", "13", "Shuttle Search")>]
let Solver = simpleFileHandler solveSilver solveGold