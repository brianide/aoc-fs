module Year2023.Day18

open System.Globalization
open System.IO
open FParsec
open Scaffold.Parsec
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions

let parse =
    let dir = anyOf "RDLU" .>> spaces1
    let dist = pint64 .>> spaces1
    let color = between (skipChar '(') (skipChar ')') (skipChar '#' >>. many1Satisfy isHex) .>> newline

    let conv color =
        let hex, dirNum = String.splitAt 5 color
        let dist = System.Int64.Parse(hex, NumberStyles.HexNumber)
        let dir = "RDLU"[int dirNum]
        dir, dist

    let line = pipe3 dir dist color (fun dir dist color -> (dir, dist), conv color)

    File.ReadAllText
    >> getParsed (many1 line)

let inline tmul m (a, b) = a * m, b * m
let inline tadd (a, b) (x, y) = a + x, b + y

let solve input =
    let foldCoords (prev, coords, len) (dir, dist) =
        let pos =
            match dir with
            | 'U' -> -dist, 0L | 'D' -> dist, 0L | 'L' -> 0L, -dist | 'R' -> 0L, dist
            | x -> failwithf "Invalid direction: %A" x
            |> tadd prev

        pos, (pos :: coords), len + dist
    
    let _, coords, length = input |> List.fold foldCoords ((0L, 0L), [], 0L)
    
    List.pairwise coords
    |> List.sumBy (fun ((x1, y1), (x2, y2)) -> x1 * y2 - y1 * x2)
    |> fun n -> (n + length) / 2L + 1L
    |> sprintf "%d"

let solveSilver = List.map fst >> solve
let solveGold = List.map snd >> solve

[<Solution("2023", "18", "Lavaduct Lagoon")>]
let Solver = chainFileHandler parse solveSilver solveGold