module Year2023.Day10

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Image

let inline tadd (a, b) (t, u) = (a + t, b + u)
let inline tmul m (a, b) = (a * m, b * m)

let parse path =
    let charGrid =
        File.ReadAllLines path
        |> Seq.map seq
        |> array2D

    let rows, cols = Array2D.length1 charGrid, Array2D.length2 charGrid
    let inline recenter a = a |> tmul 3 |> tadd (1, 1)
    let offset a offs = offs |> List.map (recenter a |> tadd)

    let mutable start = None
    let pipeGrid =
        ([], charGrid) ||> Array2D.foldi (fun acc r c ->
            function
            | '|' -> [0, 0; -1, 0; 1,  0]
            | '-' -> [0, 0;  0, 1; 0, -1]
            | 'L' -> [0, 0; -1, 0; 0,  1]
            | 'J' -> [0, 0; -1, 0; 0, -1]
            | '7' -> [0, 0;  1, 0; 0, -1]
            | 'F' -> [0, 0;  1, 0; 0,  1]
            | '.' -> []
            | 'S' -> start <- (r, c) |> recenter |> Some; [0, 0; -1, 0; 0, 1; 1, 0; 0, -1]
            | ch -> failwithf "Unknown pipe char at %d %d: %A" r c ch 
            >> offset (r, c)
            >> fun n -> n :: acc)
        |> List.collect id
        |> Set.ofList
        
    
    match start with
    | Some s -> pipeGrid, s, (rows * 3, cols * 3)
    | None -> failwith "No starting position found"

let getPath (tiles, start, dims) =
    let dir =
        [0, -1; 0, 1; -1, 0; 1, 0]
        |> List.filter (fun p -> Set.contains (p |> tmul 2 |> tadd start) tiles)
        |> List.head
        |> tadd start

    let rec traverse curr path =
        if curr = start then
            path
        else
            [0, -1; 0, 1; -1, 0; 1, 0]
            |> List.map (tadd curr)
            |> List.filter (fun p -> p <> List.head path && Set.contains p tiles)
            |> List.exactlyOne
            |> fun p -> traverse p (curr :: path)
    
    traverse dir [start]
    |> List.rev
    |> fun p -> p, dims, tiles

let solveSilver (path, _, _) =
    List.length path / 6
    |> sprintf "%d"

type Dir = N | S | E | W

module Colors =
    let Black = 0x070600 |> intToRGB
    let Salmon = 0xEA526F |> intToRGB
    let White = 0xF7F7FF |> intToRGB
    let Cyan = 0x23B5D3 |> intToRGB
    let Blue = 0x279AF1 |> intToRGB

let solveGold (path, (rows, cols), tiles) =
    let mutable frame = 0
    let framenum () =
        let f = sprintf ".frames/%06d.ppm" frame
        frame <- frame + 1
        f

    let segments = 
        List.pairwise path
        |> List.map (fun (a, b) -> b |> tmul -1 |> tadd a)
        |> List.map (function
            |  0,  1 -> E
            |  0, -1 -> W
            |  1,  0 -> S
            | -1,  0 -> N
            | x -> failwithf "Invalid path segment: %A" x)
    
    let turns = 
        List.pairwise segments
        |> List.map (function
            | N, E | E, S | S, W | W, N -> 1
            | N, W | W, S | S, E | E, N -> -1
            | t, u when t = u -> 0
            | x -> failwithf "Invalid turn: %A" x)
        |> List.reduce (+)

    let fillBegin =
        match turns, (List.item 2 segments) with
        | t, N when t < 0 ->  0,  1
        | t, N when t > 0 ->  0, -1
        | t, S when t < 0 ->  0, -1
        | t, S when t > 0 ->  0,  1
        | t, E when t < 0 ->  1,  0
        | t, E when t > 0 -> -1,  0
        | t, W when t < 0 -> -1,  0
        | t, W when t > 0 ->  1,  0
        | x -> failwithf "Unhandled start case: %A" x
        |> tadd (List.item 2 path)

    let pipes = set tiles
    let pathset = set path

    // 2D array to keep track of visited tiles
    let visited = Array2D.create rows cols false
    for i, (r, c) in List.indexed path do
        visited[r, c] <- true
        if i % 72 = 0 then
            saveToPPM cols rows (framenum ()) (fun cr cc ->
                if visited[cr, cc] then Colors.Cyan
                else if Set.contains (cr, cc) pipes then intToRGB 0x2f3030
                else Colors.Black)

    // Floodfill
    let rec bfs queue depth =
        if List.isEmpty queue then
            depth
        else
            printfn "%A" queue.Length
            let queueset = set queue
            saveToPPM cols rows (framenum ()) (fun r c ->
                if Set.contains (r, c) queueset then
                    Colors.White
                else if Set.contains (r, c) pathset then
                    Colors.Blue
                else if visited[r, c] then
                    Colors.Cyan
                else
                    Colors.Black)
            [ for p in queue do
                for off in [-1, 0; 1, 0; 0, -1; 0, 1] do
                    let (r, c) = tadd p off
                    if visited[r, c] = false then (r, c)]
            |> fun s -> s |> set |> Set.toList
            |> (fun ps ->
                for (r, c) in ps do visited[r, c] <- true
                ps, depth + 1)
            ||> bfs
    
    bfs [fillBegin] 0 |> ignore

    // Remove tiles adjacent to pipes
    for i, (r, c) in List.indexed path do
        for ro in -1..1 do
            for co in -1..1 do
                if ro <> 0 || co <> 0 then
                    visited[r + ro, c + co] <- false
                    
        if i % 144 = 0 then
            saveToPPM cols rows (framenum ()) (fun cr cc ->
                if Set.contains (cr, cc) pathset then Colors.Blue
                else if visited[cr, cc] then Colors.White
                else Colors.Black)

    saveToPPM cols rows (framenum ()) (fun cr cc ->
            if visited[cr, cc] then Colors.White
            else Colors.Black)

    Array2D.fold (fun acc v -> if v then acc + 1 else acc) 0 visited / 9
    |> sprintf "%d"

[<Solution("2023", "10", "Pipe Maze")>]
let Solver = chainFileHandler (parse >> getPath) solveSilver solveGold