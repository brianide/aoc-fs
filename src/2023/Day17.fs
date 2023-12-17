module Year2023.Day17

open System.IO
open System.Collections.Generic
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Patterns

let parse =
    File.ReadAllLines
    >> Seq.map (Seq.map (fun ch -> ch - '0' |> int))
    >> array2D

let inline (.+) (a, b) (x, y) = a + x, b + y

type State =
    { Pos: int * int
      Dir: int * int
      Moves: int
      Lost: int }

let solveSilver grid =
    let rows, cols = Array2D.dimensions grid
    let inside (r, c) = 0 <= r && r < rows && 0 <= c && c < cols
    let neighbors state =
        seq {
            if state.Moves < 3 then
                yield { state with Pos = state.Pos .+ state.Dir; Moves = state.Moves + 1 }

            match state.Dir with
            | 0, 1 | 0, -1 -> for i in [1, 0; -1, 0] do yield { state with Dir = i; Pos = state.Pos .+ i; Moves = 0 }
            | 1, 0 | -1, 0 -> for i in [0, 1; 0, -1] do yield { state with Dir = i; Pos = state.Pos .+ i; Moves = 0 }
            | 0, 0 -> for i in [0, 1; 1, 0] do yield { state with Dir = i; Pos = state.Pos .+ i; Moves = 0 }
            | x -> failwithf "Invalid direction: %A" x
        }
        |> Seq.filter (_.Pos >> inside)
        |> Seq.map (fun state ->
            let r, c = state.Pos
            { state with Lost = state.Lost + grid[r, c] })

    let pq = PriorityQueue<_,_>()
    let dist = Dictionary<_,_>()
    let init = { Pos = 0, 0; Dir = 0, 0; Moves = 0; Lost = 0 }
    let fin = rows - 1, cols - 1
    pq.Enqueue(init, 0)

    while dist.ContainsKey fin |> not && pq.Count > 0 do
        let head = pq.Dequeue()
        for next in neighbors head do
            match dist.TryGetValue next.Pos with
            | true, d when next.Lost < d -> next.Lost
            | true, d -> d
            | false, _ -> pq.Enqueue(next, next.Lost); next.Lost
            |> fun n -> dist[next.Pos] <- n
    
    dist.GetValueOrDefault(fin, -1)
    |> sprintf "%d" 

let solveGold grid =
    "Not implemented"

[<Solution("2023", "17", "Clumsy Crucible")>]
let Solver = chainFileHandler parse solveSilver solveGold