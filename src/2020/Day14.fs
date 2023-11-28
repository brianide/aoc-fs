module Year2020.Day14

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Patterns

type Instruction =
| SetMask of uint64 * uint64
| Write of uint64 * uint64

let parse path =
    let rec parseMask mask value = function
    | 'X' :: rest -> parseMask (mask <<< 1) (value <<< 1) rest
    | '1' :: rest -> parseMask (mask <<< 1 ||| 1UL) (value <<< 1 ||| 1UL) rest
    | '0' :: rest -> parseMask (mask <<< 1 ||| 1UL) (value <<< 1) rest
    | [] -> SetMask (mask, value)
    | c :: rest -> failwithf "Invalid character: %A" c

    let parseLine = function
    | RegGroups @"^mask = ([01X]+)$" [mask] -> Seq.toList mask |> parseMask 0UL 0UL
    | RegGroups @"^mem\[(\d+)] = (\d+)$" [TryParse uint64 addr; TryParse uint64 value] -> Write (addr, value)
    | x -> failwithf "Invalid line: %s" x

    File.ReadAllLines path
    |> Seq.map parseLine
    |> Seq.toList

let solveSilver input =
    let folder ((mask, value), mem) = function
    | SetMask (mask, value) -> ((mask, value), mem)
    | Write (addr, v) -> ((mask, value), Map.add addr ((mask &&& value) ||| (~~~mask &&& v)) mem)
    
    List.fold folder ((0UL, 0UL), Map.empty) input
    |> snd
    |> Map.values
    |> Seq.reduce (+)
    |> sprintf "%i"

let solveGold input =
    "Not implemented"

[<Solution("2020", "14", "Docking Data")>]
let Solver = chainFileHandler parse solveSilver solveGold