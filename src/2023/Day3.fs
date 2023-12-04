module Year2023.Day3

open System.IO
open System.Text.RegularExpressions
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Patterns

#nowarn "25"

type Comp = Symbol of string | Number of int

let digits n =
    let rec helper i = function
    | 0 -> i
    | n -> helper (i + 1) (n / 10)
    helper 0 n

let parse path =
    let parseLine r line =
        Regex(@"(\d+)|[^.]").Matches(line)
        |> Seq.map (fun m -> match m.Index, m.Value with c, Int32 n -> (r, c, Number n) | c, ch -> (r, c, Symbol ch))

    File.ReadAllLines path
    |> Seq.mapi parseLine
    |> Seq.collect id

let solveSilver input =
    let nums, syms =
        input
        |> Seq.groupBy (function (_, _, Number _) -> 0 | _ -> 1)
        |> Seq.sortBy fst
        |> Seq.map snd
        |> Seq.toPair

    seq {
        for (rs, cs, Symbol _) in syms do
            for (rn, cn, Number n) in nums do
                let rd = abs(rs - rn)
                let cd = cs - cn
                if rd <= 1 && cd >= -1 && cd <= digits n then
                    yield ((rn, cn), n)
    }
    |> set
    |> Seq.sumBy snd
    |> string

let solveGold input =
    let nums, syms =
        input
        |> Seq.groupBy (function (_, _, Number _) -> 0 | _ -> 1)
        |> Seq.sortBy fst
        |> Seq.map snd
        |> Seq.toPair

    seq {
        for (rs, cs, Symbol _) in syms do
            for (rn, cn, Number n) in nums do
                let rd = abs(rs - rn)
                let cd = cs - cn
                if rd <= 1 && cd >= -1 && cd <= digits n then
                    yield ((rs, cs), n)
    }
    |> Seq.groupBy fst
    |> Seq.map snd
    |> Seq.filter (fun s -> Seq.length s = 2)
    |> Seq.sumBy (Seq.map snd >> Seq.reduce (*))
    |> string

[<Solution("2023", "3", "Gear Ratios")>]
let Solver = chainFileHandler parse solveSilver solveGold