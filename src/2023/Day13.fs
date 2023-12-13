module Year2023.Day13

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions

let parse =
    File.ReadAllLines
    >> Seq.fold (fun acc line ->
        match acc, line with
        | groups, "" -> [] :: groups
        | group :: rest, line -> (Seq.toArray line :: group) :: rest
        | x -> failwithf "Invalid state: %A" x) [[]]
    >> Seq.map (Seq.rev >> Seq.toArray)
    >> Seq.rev

let matchLengths (a: 'a[]) (b: 'a[]) =
    if a.Length < b.Length then
        a, Array.take a.Length b
    else
        Array.take b.Length a, b

let solveGrid (grid: char[][]) =
    let isPivot grid i = 
        Array.splitAt i grid
        |> fun (a, b) -> Array.rev a, b
        ||> matchLengths
        ||> Array.forall2 (=)

    [1 .. grid.Length - 1]
    |> List.tryFind (isPivot grid)       
    |> function
    | Some n -> n * 100
    | None ->
        let xpose = Array.transpose grid
        [1 .. xpose.Length - 1]
        |> List.tryFind (isPivot xpose)
        |> function
        | Some n -> n
        | None -> failwithf "No inflection point found:\n%A" grid

let solveSilver input =
    Seq.sumBy solveGrid input
    |> sprintf "%d"

let diffGrid (grid: char[][]) =
    let diffOver grid i = 
        Array.splitAt i grid
        |> fun (a, b) -> Array.rev a, b
        ||> matchLengths
        ||> Array.map2 (fun a b ->
            (a, b)
            ||> Array.map2 (fun a b ->
                if a <> b then
                    Some i
                else
                    None)
            |> Seq.choose id)
        |> Seq.collect id

    [1 .. grid.Length - 1]
    |> List.map (diffOver grid)
    |> Seq.tryPick Seq.tryExactlyOne
    |> function
    | Some n -> n * 100
    | None ->
        let xpose = Array.transpose grid
        [1 .. xpose.Length - 1]
        |> List.map (diffOver xpose)
        |> Seq.tryPick Seq.tryExactlyOne
        |> function
        | Some n -> n
        | None -> failwithf "No inflection point found:\n%A" grid

let solveGold input =
    Seq.sumBy diffGrid input
    |> sprintf "%d"

[<Solution("2023", "13", "Point of Incidence")>]
let Solver = chainFileHandler parse solveSilver solveGold