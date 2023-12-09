module Year2023.Day9

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions

let parse =
    File.ReadAllLines
    >> Seq.map (_.Split(' ') >> Seq.map int >> Seq.toList)
    >> Seq.toList

let solve input =
    let diffs nums = List.pairwise nums |> List.map (fun (a, b) -> a - b)
    let isConstant nums = List.pairwise nums |> List.forall (fun (a, b) -> a = b)
    let rec genRows nums stack =
        if isConstant nums then
            nums :: stack
        else
            let next = diffs nums
            genRows next (nums :: stack)
        
    let solveLine nums =
        genRows nums []
        |> List.map (List.head)
        |> List.reduce (+)
        
    List.map solveLine input
    |> List.sum
    |> sprintf "%d"

[<Solution("2023", "9", "Mirage Maintenance")>]
let Solver = chainFileHandler parse (List.map List.rev >> solve) solve