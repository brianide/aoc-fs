module Year2023.Day16

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions

let parse =
    File.ReadAllLines
    >> array2D

let inline (.+) (a, b) (x, y) = a + x, b + y

let printGrid (grid: char[,]) seen =
    for r in 0 .. Array2D.length1 grid - 1 do
        for c in 0 .. Array2D.length2 grid - 1 do
            if Set.contains (r, c) seen then
                printf "#"
            else
                printf "%c" grid[r, c]
        printfn ""

let solve start (grid: char[,]) =
    let rows, cols = Array2D.dimensions grid
    let inside (r, c) = 0 <= r && r < rows && 0 <= c && c < cols
    let neighbors dir (r, c) =
        match grid[r, c], dir with
        | '.', dir -> [dir]
        | '/', (0, 1) -> [-1, 0]
        | '/', (-1, 0) -> [0, 1]
        | '/', (0, -1) -> [1, 0]
        | '/', (1, 0) -> [0, -1]
        | '\\', (0, 1) -> [1, 0]
        | '\\', (1, 0) -> [0, 1]
        | '\\', (0, -1) -> [-1, 0]
        | '\\', (-1, 0) -> [0, -1]
        | '-', dir when dir = (0, 1) || dir = (0, -1) -> [dir]
        | '-', (r, c) -> [c, r; c, -r]
        | '|', dir when dir = (1, 0) || dir = (-1, 0) -> [dir]
        | '|', (r, c) -> [c, r; -c, r]
        | x -> failwithf "Unhandled case: %A" x
        |> List.map (fun d -> d, d .+ (r, c))
        |> List.filter (snd >> inside)

    let rec bfs queue seen =
        if Set.isEmpty queue then
            seen
        else
            seq {
                for (dir, pos) in queue do
                    for p in neighbors dir pos do
                        if not <| Set.contains p seen then
                            yield p }
            |> set
            |> fun next -> next, Set.union next seen
            ||> bfs
    
    let init = Set.singleton start
    bfs init init
    |> Seq.map snd
    |> set
    |> Set.count

let solveSilver = solve ((0, 1), (0, 0)) >> sprintf "%d"

let solveGold grid =
    let rows, cols = Array2D.dimensions grid
    seq {
        for r in 0 .. rows - 1 do
            yield (0, 1), (r, 0)
            yield (0, -1), (r, cols - 1)
        for c in 0 .. cols - 1 do
            yield (1, 0), (0, c)
            yield (-1, 0), (rows - 1, c)
    }
    |> Seq.map (fun s -> solve s grid)
    |> Seq.max
    |> sprintf "%d"

[<Solution("2023", "16", "The Floor Will Be Lava")>]
let Solver = chainFileHandler parse solveSilver solveGold