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
    let rows, cols = Array2D.dimensions grid
    let twrap (r, c) =
        let r' = (r % rows + rows) % rows
        let c' = (c % cols + cols) % cols
        r', c'

    let neighbors p =
        [ for off in [0, 1; 1, 0; 0, -1; -1, 0] do
            let po = tadd p off
            let wr, wc =  twrap po
            if grid[wr, wc] <> '#' then po ]

    let reps = 65 + 131 * 2
    let rec bfs queue seen dist =
        if dist = 0 then
            seen
        else
            set [ for p in queue do yield! neighbors p ]
            |> Set.filter (fun p -> Map.containsKey p seen |> not)
            |> fun ns ->
                let seen = Seq.fold (fun acc p -> Map.add p (reps - dist) acc) seen ns
                bfs ns seen (dist - 1)
    
    bfs (Set.singleton start) Map.empty reps
    |> Map.filter (fun _ v -> v % 2 <> reps % 2)
    |> Map.count
    |> sprintf "%d"

let solveGold (grid: char[,], start) =
    let rows, cols = Array2D.dimensions grid
    "Not implemented"

[<Solution("2023", "21", "Step Counter")>]
let Solver = chainFileHandler parse solveSilver solveGold