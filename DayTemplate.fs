module Year!YEAR!.Day!DAY!

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Patterns

let parse path =
    "Not implemented"

let solveSilver input =
    "Not implemented"

let solveGold input =
    "Not implemented"

[<Solution("!YEAR!", "!DAY!", "!NAME!")>]
let Solver = chainFileHandler parse solveSilver solveGold