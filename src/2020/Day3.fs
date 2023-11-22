module Year2020.Day3

open System.IO
open Scaffold.Handlers

let parse path =
    File.ReadAllLines path
    |> Array.map Seq.toArray
    |> array2D

let countTrees xvel yvel grid =
    let width, height = Array2D.length2 grid, Array2D.length1 grid
    let rec count x y total =
        if y >= height then
            total
        else
            let total = total + (if grid[y, x] = '#' then 1 else 0)
            let x = (x + xvel) % width
            let y = y + yvel
            count x y total
    
    count 0 0 0

let solveSilver = countTrees 3 1 >> sprintf "%A"

let solveGold grid =
    [1, 1; 3, 1; 5, 1; 7, 1; 1, 2]
    |> List.map (fun (x, y) -> countTrees x y grid)
    |> List.reduce (*)
    |> sprintf "%A"

let Solver = chainFileHandler parse solveSilver solveGold