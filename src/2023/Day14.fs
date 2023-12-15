module Year2023.Day14

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions

let parse =
    File.ReadAllLines
    >> Seq.map Seq.toList
    >> Seq.toList

/// Roll stones toward the end of each row (ie. to the right).
let rollStones =
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

    List.map (roll [] [])

/// Count weight along the row axis (ie. to the right).
let calcWeight =
    Seq.sumBy (Seq.mapi (fun i ch -> if ch = 'O' then i + 1 else 0) >> Seq.sum)

let rotRight =
    List.transpose
    >> List.map List.rev

let solveSilver =
    rotRight
    >> rollStones
    >> calcWeight
    >> sprintf "%d"

let solveGold grid =
    let gen =
        rollStones
        >> rotRight >> rollStones
        >> rotRight >> rollStones
        >> rotRight >> rollStones
        >> rotRight
        >> fun g -> Some (calcWeight g, g)

    Seq.unfold gen (rotRight grid)
    |> Seq.take 1000
    |> Seq.map (sprintf "%d")
    |> String.concat "\n"

[<Solution("2023", "14", "Parabolic Reflector Dish")>]
let Solver = chainFileHandler parse solveSilver solveGold