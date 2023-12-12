module Year2023.Day12

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Patterns

#nowarn "25"

let parse =
    let parseLine (line: string) =
        let [| springs; nono |] = line.Split(' ')
        let temp = Seq.map (function '#' -> 1 | _ -> 0) springs |> Seq.toArray |> Array.insertAt 0 0
        let wilds = Seq.indexed springs |> Seq.collect (function i, '?' -> [i] | _ -> []) |> Seq.toList
        let nono = nono |> _.Split(',') |> Seq.map int |> Seq.toList
        temp, wilds, nono

    File.ReadAllLines
    >> Seq.map parseLine
    >> Seq.toList

let groups line =
    Seq.partitionBy id line
    |> Seq.filter (Seq.head >> (=) 1)
    |> Seq.map Seq.length
    |> Seq.toList

let perms (line: int[]) wilds =
    let rec helper wilds = seq {
        match wilds with
        | ind :: more ->
            line[ind + 1] <- 0
            yield! helper more
            line[ind + 1] <- 1
            yield! helper more
        | [] ->
            yield line |> Array.toList
    }
    helper wilds
    |> Seq.toList


let solveSilver input =
    input
    |> Seq.sumBy (fun (temp, wilds, nono) ->
        perms temp wilds
        |> List.map groups
        |> List.filter ((=) nono)
        |> List.length)
    |> sprintf "%d"

let solveGold input =
    "Not implemented"

[<Solution("2023", "12", "Hot Springs")>]
let Solver = chainFileHandler parse solveSilver solveGold