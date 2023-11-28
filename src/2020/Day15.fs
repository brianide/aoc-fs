module Year2020.Day15

open System.IO
open System.Collections.Generic
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
    let mutable log = Dictionary<int, int>()

    let unfolder (prev, ind) =
        let say =
            if ind < len then
                Seq.item ind input
            else
                match log.TryGetValue prev with
                | true, j -> ind - j
                | false, _ -> 0        
        log[prev] <- ind
        Some (say, (say, ind + 1))

    Seq.unfold unfolder (-1, 0)
    |> Seq.item (index - 1)
    |> sprintf "%i"

[<Solution("2020", "15", "Rambunctious Recitation")>]
let Solver = chainFileHandler parse (solve 2020) (solve 30000000)