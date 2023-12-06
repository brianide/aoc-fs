module Year2023.Day6

open System.IO
open FParsec
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Patterns

let parse path =
    let ws = skipMany (skipChar ' ')
    let nums = many1 (pint64 .>> ws) .>> newline
    let spec =
        tuple2 (skipString "Time:" >>. spaces >>. nums) (skipString "Distance:" >>. spaces >>. nums)
        |>> fun (a, b) -> Seq.zip a b
    
    File.ReadAllText path
    |> run spec 
    |> function
    | Success (res, _, _) -> res
    | Failure (msg, _, _) -> failwith msg

let binarySearch fn min max target =
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

let countWays (time, dist) =
    binarySearch (fun n -> n * (time - n)) 1L (time / 2L) dist
    |> fun n -> time - 2L * n + 1L

let solveSilver =
    Seq.map countWays
    >> Seq.reduce (*)
    >> sprintf "%d"

let solveGold =
    let countDigits n =
        let rec helper i = function
        | 0L -> i
        | n -> helper (i + 1) (n / 10L)
        helper 0 n

    let combineDigits (a, b) (c, d) =
        let comb x y = (pown 10 (countDigits y) |> int64) * x + (int64 y)
        comb a c, comb b d

    Seq.fold combineDigits (0L, 0L)
    >> countWays
    >> sprintf "%d"

[<Solution("2023", "6", "Wait For It")>]
let Solver = chainFileHandler parse solveSilver solveGold