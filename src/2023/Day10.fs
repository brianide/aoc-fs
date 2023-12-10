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
    |> fun p -> p, dims

let solveSilver (path, _) =
    List.length path / 6
    |> sprintf "%d"

type Dir = N | S | E | W

let solveGold (path, (rows, cols)) =
    let mutable frame = 0
    let framenum () =
        let f = sprintf ".frames/%06d.pgm" frame
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

    // 2D array to keep track of visited tiles
    let visited = Array2D.create rows cols false
    for i, (r, c) in List.indexed path do
        visited[r, c] <- true
        if i % 72 = 0 then
            saveToPGM cols rows (framenum ()) (fun cr cc ->
                if (r, c) = (cr, cc) then byte 255
                else if visited[cr, cc] then byte 127
                else byte 0)

    // Floodfill
    let rec bfs queue depth =
        if List.isEmpty queue then
            depth
        else
            printfn "%A" queue.Length
            let queueset = set queue
            saveToPGM cols rows (framenum ()) (fun r c ->
                if Set.contains (r, c) queueset then
                    byte 255
                else if visited[r, c] then
                    byte 127
                else
                    byte 0)
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
                    
        if i % 72 = 0 then
            saveToPGM cols rows (framenum ()) (fun cr cc ->
                            if (r, c) = (cr, cc) then byte 255
                            else if visited[cr, cc] then byte 127
                            else byte 0)


    Array2D.fold (fun acc v -> if v then acc + 1 else acc) 0 visited / 9
    |> sprintf "%d"

[<Solution("2023", "10", "Pipe Maze")>]
let Solver = chainFileHandler (parse >> getPath) solveSilver solveGold