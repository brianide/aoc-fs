module Year2023.Day14

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions

let parse =
    File.ReadAllLines
    >> array2D

let solveSilver (input: char[,]) =
    let rec roll run acc col =
        match col, run with
        | '.' :: rest, run -> roll ('.' :: run) acc rest
        | 'O' :: rest, run -> roll ('O' :: run) acc rest
        | '#' :: rest, run ->
            roll ('#' :: run) acc []
            |> fun s -> roll [] s rest
        | [], [] -> acc
        | [], run ->
            run
            |> List.sortBy (function 'O' -> 1 | '#' -> 2 | _ -> 0)
            |> fun s -> List.concat [acc; s]
        | x -> failwithf "Invalid state:\n%A" x

    seq { for i in 0 .. Array2D.length2 input - 1 do List.ofArray input[*,i] |> List.rev }
    |> Seq.map (roll [] [] >> List.mapi (fun i ch -> if ch = 'O' then i + 1 else 0))
    |> Seq.sumBy Seq.sum
    |> sprintf "%A"

let solveGold input =
    "Not implemented"

[<Solution("2023", "14", "Parabolic Reflector Dish")>]
let Solver = chainFileHandler parse solveSilver solveGold