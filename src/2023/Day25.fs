module Year2023.Day25

open System.IO
open FParsec
open Scaffold.Parsec
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions

let parse =
    let id = regex @"[a-z]{3}"
    let line = tuple2 (id .>> skipChar ':') (many (skipChar ' ' >>. id)) .>> newline
    let map = (many1 line) |>> Map.ofList

    File.ReadAllText
    >> getParsed map

let solveSilver (graph: Map<string, List<string>>) =
    // Graphviz output
    // graph
    // |> Map.toSeq
    // |> Seq.map (fun (k, vs) -> String.concat " " vs |> sprintf "  %s -- {%s}" k)
    // |> String.concat "\n"
    // |> sprintf "graph {\n%s\n}"

    let undir =
        let comb src map dest =
            Map.update dest [src] (fun ds -> src :: ds) map
        Map.fold (fun map k cs -> List.fold (comb k) map cs) graph graph

    printf "%A" (Map.count graph)

    let keys = Map.keys undir |> set
    let rec bfs queue seen islands =
        if Set.count queue = 0 then
            Set.difference keys seen
            |> Seq.tryHead
            |> function
            | Some n -> bfs (Set.singleton n) (Set.add n seen) (Set.count seen :: islands)
            | None -> Set.count seen :: islands
        else
            [ for k in queue do
                for n in Map.find k undir do
                    if Set.contains n seen |> not then
                        n ]
            |> set
            |> fun es -> bfs es (Set.union seen es) islands
    bfs Set.empty Set.empty []
    |> sprintf "%A"

let solveGold input =
    "Not implemented"

[<Solution("2023", "25", "Snowverload")>]
let Solver = chainFileHandler parse solveSilver solveGold