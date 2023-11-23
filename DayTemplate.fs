module Year.Day

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Util.Patterns

let parse path =
    "Not implemented"

let solveSilver input =
    input

let solveGold input =
    input

[<Solution("", "", "")>]
let Solver = chainFileHandler parse solveSilver solveGold