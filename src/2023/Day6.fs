module Year2023.Day6

open System.IO
open FParsec
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Patterns

let parse path =
    let ws = skipMany (pchar ' ')
    let nums = many1 (pint32 .>> ws) .>> newline
    let spec =
        tuple2 (skipString "Time:" >>. spaces >>. nums) (skipString "Distance:" >>. spaces >>. nums)
        |>> fun (a, b) -> Seq.zip a b
    
    File.ReadAllText path
    |> run spec 
    |> function
    | Success (res, _, _) -> res
    | Failure (msg, _, _) -> failwith msg

let solveSilver input =
    let mutable prod = 1
    for (time, dist) in input do
        let mutable count = 0
        for hold in 1 .. time / 2 do
            let trav = time - hold
            if hold * trav > dist then
                count <- count + if hold = trav then 1 else 2
        prod <- prod * count
    string prod

let digits n =
    let rec helper i = function
    | 0 -> i
    | n -> helper (i + 1) (n / 10)
    helper 0 n

let binsearch fn min max target =
    let rec helper l r =
        if l = r then
            l
        else
            let m = (l + r) / 2L
            let a = fn m
            if a <= target then
                helper (m + 1L) r
            else
                helper l m
    helper min max

let solveGold input =
    let combineDigits (a, b) (c, d) =
        let comb x y = (pown 10 (digits y) |> int64) * x + (int64 y)
        comb a c, comb b d

    let countWays (time, dist) =
        binsearch (fun n -> n * (time - n)) 1L (time / 2L) dist
        |> fun n -> time - 2L * n + 1L

    Seq.fold combineDigits (0L, 0L) input
    |> countWays
    |> sprintf "%d"

[<Solution("2023", "6", "Wait For It")>]
let Solver = chainFileHandler parse solveSilver solveGold