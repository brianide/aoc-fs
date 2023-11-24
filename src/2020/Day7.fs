module Year2020.Day7

open System.IO
open System.Text.RegularExpressions
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Inspect
open Scaffold.Util.Patterns

let parse path =
    let reg = Regex @"(\d+) (.+?) bags?[,.]"
    
    let parseMatches = function
    | [ Int32 num; color ] -> color, num
    | _ -> failwith "Invalid match"

    let parseLine (line: string) =
        line.Split(" bags contain ")
        |> function
        | [| bag; "no other bags" |] ->
            (bag, [])
        | [| bag; cont |] ->
            reg.Matches(cont)
            |> Seq.toList
            |> List.map (_.Groups >> Seq.tail >> Seq.map _.Value >> Seq.toList >> parseMatches)
            |> (fun cont -> (bag, cont))
        | _ -> failwithf "Invalid line: %s" line

    File.ReadAllLines path
    |> Array.toList
    |> List.map parseLine
    |> Map

let solveSilver (map: Map<string, list<string * int>>) =
    let map =
        map
        |> Seq.collect(fun kv -> kv.Value |> Seq.map (fun a -> fst a, kv.Key))
        |> Seq.fold (fun acc (k, v) -> Map.update k [v] (fun a -> v :: a) acc) Map.empty

    let rec bfs queue visited =
        if queue = [] then
            visited
        else
            let queue = queue |> List.collect (fun k -> Map.getOrDefault k [] map |> List.filter (fun e -> not <| Set.contains e visited))
            let visited = Set.union visited (Set queue)
            bfs queue visited

    bfs ["shiny gold"] Set.empty
    |> Seq.length
    |> sprintf "%A"

let solveGold map =
    let rec dfs root =
        Map.getOrDefault root [] map
        |> List.sumBy (fun (k, c) -> c + c * dfs(k))
    dfs "shiny gold"
    |> sprintf "%A"

[<Solution("2020", "7", "Handy Haversacks")>]
let Solver = chainFileHandler parse solveSilver solveGold