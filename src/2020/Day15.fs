module Year2020.Day15

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Patterns

let parse =
    File.ReadAllText
    >> _.Split(',')
    >> Seq.map int
    >> Seq.toList

let solve index input =
    let len = Seq.length input
    let unfolder (prev, ind, log) =
        let say =
            if ind < len then
                Seq.item ind input
            else
                match Map.tryFind prev log with
                | Some j -> ind - j
                | None -> 0
        let log = Map.add prev ind log
        Some (say, (say, ind + 1, log))

    Seq.unfold unfolder (-1, 0, Map.empty)
    |> Seq.item (index - 1)
    |> sprintf "%i"

[<Solution("2020", "15", "Rambunctious Recitation")>]
let Solver = chainFileHandler parse (solve 2020) (solve 30000000)