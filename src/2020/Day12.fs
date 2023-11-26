module Year2020.Day12

open System;
open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Patterns

type Turn = L | R
type Nav =
| Move of int * int
| Rotate of int
| Fore of int

let parse path =
    let (|Nav|) line =
        match String.splitAt 1 line with
        | "N", Int32 n -> Move (0, n)
        | "S", Int32 n -> Move (0, -n)
        | "E", Int32 n -> Move (n, 0)
        | "W", Int32 n -> Move (-n, 0)
        | "L", Int32 n -> Rotate n
        | "R", Int32 n -> Rotate (360 - n)
        | "F", Int32 n -> Fore n
        | _ -> failwithf "Invalid instruction: %A" line

    File.ReadAllLines path
    |> Array.toList
    |> List.map (|Nav|)

module Silver =
    let icos = function 0 -> 1 | 90 -> 0 | 180 -> -1 | 270 -> 0 | x -> failwithf "Invalid input: %i" x
    let isin = function 0 -> 0 | 90 -> 1 | 180 -> 0 | 270 -> -1 | x -> failwithf "Invalid input: %i" x

    let solve input =
        let folder (dir, x, y) = function
        | Move (dx, dy) -> (dir, x + dx, y + dy)
        | Rotate n -> ((dir + n) % 360, x, y)
        | Fore n -> (dir, x + n * icos dir, y + n * isin dir)

        List.fold folder (0, 0, 0) input
        |> fun (_, x, y) -> abs x + abs y |> string

let solveGold input =
    let rotate x y = function
    | 0 -> x, y
    | 90 -> -y, x
    | 180 -> -x, -y
    | 270 -> y, -x
    | x -> failwithf "Invalid angle: %i" x

    let folder ((x, y), (bx, by)) = function
    | Move (dx, dy) -> ((x, y), (bx + dx, by + dy))
    | Rotate angle -> ((x, y), rotate bx by angle)
    | Fore n -> ((x + bx * n, y + by * n), (bx, by))

    List.fold folder ((0, 0), (10, 1)) input
    |> fun ((x, y), _) -> abs x + abs y |> string

[<Solution("2020", "12", "Rain Risk")>]
let Solver = chainFileHandler parse Silver.solve solveGold