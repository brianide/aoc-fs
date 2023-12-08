module Year2023.Day8

open System.IO
open System.Collections.Generic
open FParsec
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Parsec

let parse =
    let str = skipString
    let directions = manyTill anyChar newline .>> newline
    let nodeName = anyString 3 .>> skipString " = "
    let nodeChildren = str "(" >>. anyString 3 .>> skipString ", " .>>. anyString 3 .>> str ")"
    let nodeLine = nodeName .>>. nodeChildren .>> newline
    let spec = tuple2 directions (many1 nodeLine |>> Map)

    File.ReadAllText
    >> getParsed spec

let rec repeat coll = seq {
    yield! coll
    yield! repeat coll
}

let solveSilver (dirs, adj) =
    let mutable curr = "AAA"
    let mutable count = 0
    let mutable dirs = (repeat dirs).GetEnumerator()

    while curr <> "ZZZ" do
        dirs.MoveNext() |> ignore
        let left, right = Map.find curr adj
        curr <- if dirs.Current = 'L' then left else right
        count <- count + 1
    sprintf "%d" count

let solveSilverSlow (dirs, adj) =
    let gen (curr, dirs) =
        let left, right = Map.find curr adj
        let next = if Seq.head dirs = 'L' then left else right
        Some (next, (next, Seq.tail dirs))

    ("AAA", repeat dirs)
    |> Seq.unfold gen
    |> Seq.findIndex ((=) "ZZZ")
    |> (+) 1
    |> sprintf "%A"

let lcm a b =
    let rec gcd a b =
        if b = 0L then
            a
        else
            gcd b (a % b)

    a * b / gcd a b

let findLoop (dirs, adj) start =
    let mutable curr = start
    let mutable count = 0L
    let mutable dirs = (dirs |> Seq.indexed |> repeat).GetEnumerator()
    let mutable fin = false
    let seen = HashSet<int * string>()

    while not fin do
        dirs.MoveNext() |> ignore
        let (ind, dir) = dirs.Current
        let state = ind, curr
        if seen.Contains state then
            fin <- true
        else
            seen.Add state |> ignore
            let left, right = Map.find curr adj
            curr <- if dir = 'L' then left else right
            count <- count + 1L
    
    dirs.Current, curr, count - 1L

let solveGold (dirs, adj) =
    Map.keys adj |> Seq.filter (fun n -> (n: string).EndsWith('A'))
    |> Seq.map (findLoop (dirs, adj))
    |> Seq.map (fun ((off, _), _, len) -> len - int64 (off - 1))
    |> Seq.reduce lcm
    |> sprintf "%d"

[<Solution("2023", "8", "Haunted Wasteland")>]
let Solver = chainFileHandler parse solveSilver solveGold