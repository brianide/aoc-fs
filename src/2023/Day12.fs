module Year2023.Day12

open System.IO
open System.Collections.Generic
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions

let parse reps =
    let parseLine (line: string) =
        let tokens = line.Split(' ')
        let springs = tokens[0] |> Seq.replicate reps |> String.concat "?" |> Seq.toList
        let groups = tokens[1] |> Seq.replicate reps |> String.concat "," |> _.Split(',') |> Seq.map int |> Seq.toList
        springs, groups

    File.ReadAllLines
    >> Seq.map parseLine
    >> Seq.toList

let countPerms =
    let cache = Dictionary<_,_>()

    let rec helper state =
        match cache.TryGetValue state with
        | true, v -> v
        | false, _ -> 
            match state with
            | '.' :: springs, 0, groups -> helper (springs, 0, groups)
            | '.' :: springs, n, m :: groups when n = m -> helper (springs, 0, groups)
            | '.' :: _, _, _ -> 0L
            | '#' :: springs, n, m :: groups when n < m -> helper (springs, n + 1, m :: groups)
            | '#' :: _, _, _ -> 0L
            | '?' :: springs, n, groups -> helper ('.' :: springs, n, groups) + helper ('#' :: springs, n, groups)
            | [], 0, [] -> 1L
            | [], n, [m] when n = m -> 1L
            | [], _, _ -> 0L
            | x -> failwithf "Unhandled case: %A" x
            |> fun res -> cache.Add(state, res); res

    fun (springs, groups) -> helper (springs, 0, groups)

let solve reps =
    parse reps
    >> Seq.sumBy countPerms
    >> sprintf "%d"

[<Solution("2023", "12", "Hot Springs")>]
let Solver = simpleFileHandler (solve 1) (solve 5)