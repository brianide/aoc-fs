module Year2023.Day23

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions

type Node =
| Start
| Junction of int * int
| Finish

type Graph = Map<Node, List<Node * int>>

let parse =
    File.ReadAllLines >> array2D

let getJunctionGraph grid =
    let rows, cols = Array2D.dimensions grid
    let inline tadd (a, b) (x, y) = a + x, b + y
    let inline inside (r, c) = 0 <= r && r < rows && 0 <= c && c < cols

    let neighbors p =
        [ for off in [0, 1; 0, -1; 1, 0; -1, 0] do
            let r, c = tadd p off
            if inside (r, c) && grid[r, c] <> '#' then (r, c), grid[r, c] ]

    let getJunctionInfo (p, ch) =
        let center =
            match ch with | 'v' -> 1, 0 | '>' -> 0, 1 | x -> failwithf "Unhandled char: %A" x
            |> tadd p

        let exits =
            [ for off in [0, 1; 1, 0] do
                let re, ce = tadd center off
                if grid[re, ce] <> '#' then re, ce ]

        center, exits

    let start = 0, 1
    let finish = rows - 1, cols - 2

    let rec dfs lastJunc dist prev curr = 
        if curr = finish then
            [lastJunc, (Finish, dist - 1)]
        else
            [ for p, ch in neighbors curr do if p <> prev then p, ch ]
            |> List.exactlyOne
            |> function
            | p, '.' -> dfs lastJunc (dist + 1) curr p
            | junc ->
                let center, exits = getJunctionInfo junc
                let junc = Junction center
                let subs = List.collect (dfs junc 2 center) exits
                (lastJunc, (junc, dist + 1)) :: subs

    dfs Start 1 (-1, -1) start
    |> set
    |> Seq.fold (fun map (k, v) -> Map.update k [v] (fun vs -> v :: vs) map) Map.empty

let solveSilver (graph: Graph) =
    let rec dfs curr length =
        if curr = Finish then
            length
        else
            List.max [ for (next, dist) in Map.find curr graph do dfs next (length + dist) ]

    dfs Start 0
    |> sprintf "%d"

let solveGold (graph: Graph) =
    let undir =
        let comb src map (dest, dist) =
            if src = Start then
                map
            else
                match dest with
                | Start | Finish -> map
                | Junction _ -> Map.update dest [src, dist] (fun ds -> (src, dist) :: ds) map

        Map.fold (fun map k cs -> List.fold (comb k) map cs) graph graph
    
    let rec dfs curr length seen =
        if Set.contains curr seen then
            0
        else if curr = Finish then
            length
        else
            let seen = Set.add curr seen
            List.max [ for (next, dist) in Map.find curr undir do dfs next (length + dist) seen ]

    dfs Start 0 Set.empty
    |> sprintf "%d"

[<Solution("2023", "23", "A Long Walk")>]
let Solver = chainFileHandler (parse >> getJunctionGraph) solveSilver solveGold