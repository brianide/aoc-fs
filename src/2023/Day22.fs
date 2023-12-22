module Year2023.Day22

open System.IO
open System.Collections.Generic
open FParsec
open Scaffold.Parsec
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions

let inline thd (_, _, z) = z

let isColinear (a, b, _) (x, y, _) = a = x && b = y

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
    let bricks = (many1 brick) |>> List.sortBy fst

    File.ReadAllText
    >> getParsed bricks

let solveSilver input =
    let tops = Dictionary<_,_>()
    let suppBy = Dictionary<_,_>()

    for (i, (_, verts)) in List.indexed input do
        let zv, supports =
            verts
            |> List.choose (fun (x, y, _) -> tops.TryFind (x, y))
            |> List.groupBy snd
            |> List.sortByDescending fst
            |> List.tryHead
            |> Option.map (fun (h, ts) -> h + 1, List.map fst ts |> set)
            |> Option.defaultValue (0, Set.empty)

        for ind in supports do suppBy.Update i [ind] (fun inds -> ind :: inds)
        for (x, y, zo) in verts do tops[(x, y)] <- i, zv + zo

    suppBy.Values
    |> Seq.choose Seq.tryExactlyOne
    |> set
    |> Set.difference (set [0 .. List.length input - 1])
    |> Set.count
    |> sprintf "%d"

let solveGold input =
    "Not implemented"

[<Solution("2023", "22", "Sand Slabs")>]
let Solver = chainFileHandler parse solveSilver solveGold