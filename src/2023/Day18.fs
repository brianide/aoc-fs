module Year2023.Day18

open System.IO
open FParsec
open Scaffold.Parsec
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Patterns

let parse =
    let dir = anyOf "LRUD" .>> spaces1
    let dist = pint32 .>> spaces1
    let color = between (skipChar '(') (skipChar ')') (skipChar '#' >>. many1Satisfy isHex) .>> newline
    let line = tuple3 dir dist color
    let spec = many1 line

    File.ReadAllText
    >> getParsed spec 

let inline tmul m (a, b) = a * m, b * m
let inline tadd (a, b) (x, y) = a + x, b + y

let solveSilver input =
    let foldCoords (prev, coords, len) (dir, dist, _) =
        let pos =
            match dir with
            | 'U' -> -dist, 0 | 'D' -> dist, 0 | 'L' -> 0, -dist | 'R' -> 0, dist
            | x -> failwithf "Invalid direction: %A" x
            |> tadd prev

        pos, (pos :: coords), len + dist
    
    let _, coords, length = input |> List.fold foldCoords ((0, 0), [], 0)
    
    List.pairwise coords
    |> List.sumBy (fun ((x1, y1), (x2, y2)) -> (x2 + x1) * (y2 - y1))
    |> fun n -> (n + length) / 2 + 1
    |> sprintf "%A"

let solveGold input =
    "Not implemented"

[<Solution("2023", "18", "Lavaduct Lagoon")>]
let Solver = chainFileHandler parse solveSilver solveGold