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

    let rec bfs queue dist =
        if dist = 0 then
            queue
        else
            set [ for (r, c) in queue do yield! neighbors r c ]
            |> fun ns -> bfs ns (dist - 1)
    
    bfs (Set.singleton start) 64
    |> Set.count
    |> (+) 1
    |> sprintf "%d"

let solveGold (grid: char[,], start) =
    let rows, cols = Array2D.dimensions grid
    Scaffold.Image.saveToPGM cols rows "foo.pgm" (fun r c -> if grid[r, c] = '#' then byte 0xFF else byte 0x00)
    "Not implemented"

[<Solution("2023", "21", "Step Counter")>]
let Solver = chainFileHandler parse solveSilver solveGold