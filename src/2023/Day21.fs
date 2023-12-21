module Year2023.Day21

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Patterns

let parse path =
    let grid =
        File.ReadAllLines path
        |> Seq.map Seq.toList
        |> array2D

    let start = Array2D.indices grid |> Seq.find (fun (r, c) -> grid[r, c] = 'S')
    grid, start

let inline tadd (a, b) (x, y) = a + x, b + y

let solveSilver (grid, start) =
    let neighbors r c =
        [ for off in [0, 1; 1, 0; 0, -1; -1, 0] do
            let (pr, pc) = tadd (r, c) off
            if Array2D.isInside pr pc grid && grid[pr, pc] = '.' then
                pr, pc ]

    let rec bfs queue seen dist =
        if dist = 0 then
            seen
        else
            set [ for (r, c) in queue do yield! neighbors r c ]
            |> Set.filter (fun p -> Map.containsKey p seen |> not)
            |> fun ns ->
                let seen = Seq.fold (fun acc p -> Map.add p dist acc) seen ns
                bfs ns seen (dist - 1)
    
    bfs (Set.singleton start) Map.empty 64
    |> Map.filter (fun _ v -> v % 2 = 1)
    |> Map.count
    |> (+) 1
    |> sprintf "%A"

let solveGold (grid: char[,], start) =
    let rows, cols = Array2D.dimensions grid
    "Not implemented"

[<Solution("2023", "21", "Step Counter")>]
let Solver = chainFileHandler parse solveSilver solveGold