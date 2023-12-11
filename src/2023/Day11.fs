module Year2023.Day11

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Image

let parse path =
    let space =
        File.ReadAllLines path
        |> Seq.map seq
        |> array2D
    
    space
    |> Array2D.foldi (fun acc r c -> function '#' -> (r, c) :: acc | _ -> acc) []
    |> fun p -> p, (Array2D.length1 space, Array2D.length2 space)

let solveSilver (gals, _) =
    let expanded =
        gals
        |> List.sortBy fst 
        |> fun l -> (0, 0) :: l
        |> List.pairwise
        |> List.fold (fun (exp, ps) ((pr, _), (r, c)) ->
            let dist = r - pr - 1 |> max 0
            let exp = exp + dist
            exp, (r + exp, c) :: ps) (0, [])
        |> snd
        |> List.sortBy snd 
        |> fun l -> (0, 0) :: l
        |> List.pairwise
        |> List.fold (fun (exp, ps) ((_, pc), (r, c)) ->
            let dist = c - pc - 1 |> max 0
            let exp = exp + dist
            exp, (r, c + exp) :: ps) (0, [])
        |> snd
        |> List.sort

    expanded
    |> Seq.tails
    |> Seq.map Seq.toList
    |> Seq.sumBy (fun l ->
        let (sr, sc) = List.head l
        List.tail l
        |> List.sumBy (fun (r, c) -> abs (r - sr) + abs (c - sc)))
    |> sprintf "%d"

let solveGold input =
    "Not implemented"

[<Solution("2023", "11", "Cosmic Expansion")>]
let Solver = chainFileHandler parse solveSilver solveGold