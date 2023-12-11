module Year2023.Day11

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions

let parse =
    File.ReadAllLines
    >> Seq.map seq
    >> array2D
    >> Array2D.foldi (fun acc r c -> function '#' -> (int64 r, int64 c) :: acc | _ -> acc) []
    >> fun p -> p

let solve factor galaxies =
    let expanded =
        galaxies
        |> List.sortBy fst
        |> fun l -> (0L, 0L) :: l
        |> List.pairwise
        |> List.map (fun ((pr, _), (r, c)) -> r - pr, c)
        |> List.scan (fun (racc, _) (dr, c) ->
            let racc = racc + if dr > 1L then (dr - 1L) * factor + 1L else dr
            racc, c) (0L, 0L)
        |> List.sortBy snd 
        |> fun l -> (0L, 0L) :: l
        |> List.pairwise
        |> List.map (fun ((_, pc), (r, c)) -> r, c - pc)
        |> List.scan (fun (_, cacc) (r, dc) ->
            let cacc = cacc + if dc > 1L then (dc - 1L) * factor + 1L else dc
            r, cacc) (0L, 0L)
        |> List.skip 2

    expanded
    |> Seq.tails
    |> Seq.map Seq.toList
    |> Seq.sumBy (fun l ->
        let (sr, sc) = List.head l
        List.tail l
        |> List.sumBy (fun (r, c) -> abs (r - sr) + abs (c - sc)))
    |> sprintf "%d"

[<Solution("2023", "11", "Cosmic Expansion")>]
let Solver = chainFileHandler parse (solve 2L) (solve 1000000L)