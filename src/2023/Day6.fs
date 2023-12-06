module Year2023.Day6

open System.IO
open FParsec
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Parsec

let parse =
    let nums = many1 (pint64 .>> space) .>> newline
    let spec =
        tuple2 (skipString "Time:" >>. spaces >>. nums) (skipString "Distance:" >>. spaces >>. nums)
        |>> fun (a, b) -> Seq.zip a b
    
    File.ReadAllText
    >> getParsed spec

let solveRace (time, targ) =
    let time', targ' = double time, double targ + 1.0
    let hold = (-time' + sqrt(time' ** 2.0 - 4.0 * targ')) / -2.0 |> ceil |> int64
    time - 2L * hold + 1L

let solveSilver =
    Seq.map solveRace
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
    >> solveRace
    >> sprintf "%d"

[<Solution("2023", "6", "Wait For It")>]
let Solver = chainFileHandler parse solveSilver solveGold