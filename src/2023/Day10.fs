module Year2023.Day10

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Image

let parse path =
    let charGrid =
        File.ReadAllLines path
        |> Seq.map seq
        |> array2D

    let rows, cols = Array2D.length1 charGrid, Array2D.length2 charGrid
    let inline inside (r, c) = 0 <= r && r < rows && 0 <= c && c < cols
    let inline (.+) (a, b) (t, u) = (a + t, b + u)
    let offset a offs = offs |> List.map ((.+) a) |> List.filter inside

    let mutable start = None
    let pipeGrid =
        charGrid |> Array2D.mapi (fun r c ->
            function
            | '|' -> [-1, 0; 1, 0]
            | '-' -> [0, 1; 0, -1]
            | 'L' -> [-1, 0; 0, 1]
            | 'J' -> [-1, 0; 0, -1]
            | '7' -> [1, 0; 0, -1]
            | 'F' -> [1, 0; 0, 1]
            | '.' -> []
            | 'S' -> start <- Some (r, c); [-1, 0; 0, 1; 1, 0; 0, -1]
            | ch -> failwithf "Unknown pipe char at %d %d: %A" r c ch 
            >> offset (r, c))
    
    match start with
    | Some s -> pipeGrid, s
    | None -> failwith "No starting position found"

let solveSilver (grid: list<int*int>[,], (sr, sc)) =
    // Take one of the two neighbors of S which actually connect back to it
    let init =
        grid[sr, sc]
        |> List.filter (fun (r, c) -> List.contains (sr, sc) grid[r, c])
        |> List.head
    
    let rec travel (r, c) prev count =
        if (r, c) = (sr, sc) then
            count
        else
            let next = grid[r, c] |> List.filter (fun n -> n <> prev)|> List.exactlyOne
            travel next (r, c) (count + 1)
    travel init (sr, sc) 1
    |> fun n -> n / 2
    |> sprintf "%d"

let solveSilverBFS (grid: list<int*int>[,], (sr, sc)) =
    // Find the two neighbors of S which actually connect back to it
    let init =
        grid[sr, sc]
        |> List.map (fun (r, c) -> grid[r, c]) |> List.filter (List.contains (sr, sc))
        |> List.collect id

    // Level-by-level BFS
    let mutable count = 0
    let rec bfs queue seen depth =
        // saveToPGM (Array2D.length2 grid) (Array2D.length1 grid) (sprintf "frames/frame_%06d.pgm" count) (fun r c ->
        //     if Set.contains (r, c) seen then byte 255 else byte 0)
        // count <- count + 1
        if List.isEmpty queue then
            depth
        else
            queue
            |> List.collect (fun (r, c) -> grid[r, c])
            |> List.filter (fun n -> Set.contains n seen |> not)
            |> fun p -> p, Set.union seen (set p), depth + 1
            |||> bfs
    bfs init (Set.singleton (sr, sc)) 1
    |> sprintf "%d"

let solveGold input =
    "Not implemented"

[<Solution("2023", "10", "Pipe Maze")>]
let Solver = chainFileHandler parse solveSilver solveGold