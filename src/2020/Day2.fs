module Year2020.Day2

open System.IO
open Scaffold.Handlers
open Scaffold.Util.Patterns

let parse path =
    let parseLine = function
    | RegGroups @"^(\d+)-(\d+) (\w): (\w+)$" [Int32 a; Int32 b; Char c; pass] -> (a, b, c, pass)
    | line -> failwithf "Nonmatching line: %s" line

    File.ReadAllLines path
    |> Array.toList
    |> List.map parseLine

let checkSilver (lo, hi, c, pass) =
    Seq.filter (fun d -> d = c) pass
    |> Seq.length
    |> (fun count -> count >= lo && count <= hi)

let checkGold (a, b, c, pass) =
    List.filter (fun x -> Seq.item (x - 1) pass = c) [a; b]
    |> List.length
    |> (=) 1

let solve checkLine =
    List.filter checkLine
    >> List.length
    >> sprintf "%A"

let Solver = chainFileHandler parse (solve checkSilver) (solve checkGold)