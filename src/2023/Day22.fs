module Year2023.Day22

open System.IO
open System.Collections.Generic
open FParsec
open Scaffold.Parsec
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Collections

let inline thd (_, _, z) = z

let parse =
    let convBrick ((a, b, c), (x, y, z)) =
        let verts =
            [ for i in a .. x do
                for j in b .. y do
                    for k in c .. z do
                        i, j, k ]
            |> List.sortBy thd
        
        let (_, _, bottom) = List.head verts
        bottom, List.map (fun (x, y, z) -> (x, y, z - bottom)) verts


    let vec = tuple3 (pint32 .>> skipChar ',') (pint32 .>> skipChar ',') pint32
    let brick = tuple2 (vec .>> skipChar '~') vec .>> newline |>> convBrick
    let bricks = (many1 brick) |>> (List.sortBy fst >> List.map snd)

    File.ReadAllText
    >> getParsed bricks

let getSupportGraph input =
    let tops = Dictionary<_,_>()
    let suppFor = Dictionary<_,_>()
    let suppBy = Dictionary<_,_>()

    for (i, verts) in List.indexed input do
        let zv, supports =
            verts
            |> List.choose (fun (x, y, _) -> tops.TryFind (x, y))
            |> List.groupBy snd
            |> List.sortByDescending fst
            |> List.tryHead
            |> Option.map (fun (h, ts) -> h + 1, List.map fst ts |> set)
            |> Option.defaultValue (0, Set.empty)

        // Update top column elements
        for (x, y, zo) in verts do tops[(x, y)] <- i, zv + zo

        // Record support relationships
        for ind in supports do
            suppFor.Update ind [i] (fun is -> i :: is)
            suppBy.Update i [ind] (fun inds -> ind :: inds)

    Map.ofDict suppBy, Map.ofDict suppFor, List.length input

let solveSilver (suppBy, _, numBricks) =
    Map.values suppBy
    |> Seq.choose Seq.tryExactlyOne
    |> set
    |> Set.difference (set [0 .. numBricks - 1])
    |> Set.count
    |> sprintf "%d"

let solveGold (suppBy, suppFor, numBricks) =
    let numDepending i =
        let rec bfs queue fallen =
            if Queue.isEmpty queue then
                Set.count fallen
            else
                let ind, queue = Queue.dequeue queue
                let fallen = Set.add ind fallen

                [ for dep in Map.getOrDefault ind [] suppFor do
                    if Map.find dep suppBy |> List.forall (fun s -> Set.contains s fallen) then
                        yield dep ]
                |> fun es -> bfs (Queue.enqueueAll es queue) fallen
        
        (bfs (Queue.singleton i) Set.empty) - 1

    List.sumBy numDepending [0 .. numBricks - 1]
    |> sprintf "%d"

[<Solution("2023", "22", "Sand Slabs")>]
let Solver = chainFileHandler (parse >> getSupportGraph) solveSilver solveGold