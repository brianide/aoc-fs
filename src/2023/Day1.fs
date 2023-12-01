module Year2023.Day1

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions

let solveSilver path =
    File.ReadAllLines path
    |> Seq.map (Seq.choose (function c when c >= '0' && c <= '9' -> Some (int <| c - '0') | _ -> None))
    |> Seq.map (fun ns -> Seq.head ns * 10 + Seq.last ns)
    |> Seq.sum
    |> string

let solveGold path =
    let (|StartsWith|_|) (prefix: string) (line: string) =
        if line.StartsWith(prefix) then
            Some StartsWith
        else
            None

    let rec parseLine (line: string) =
        seq {
            let mutable line = line
            while line.Length > 0 do
                yield line
                line <- line.Substring 1
        }
        |> Seq.choose (function
            | StartsWith "0" | StartsWith "zero" -> Some 0
            | StartsWith "1" | StartsWith "one" -> Some 1
            | StartsWith "2" | StartsWith "two" -> Some 2
            | StartsWith "3" | StartsWith "three" -> Some 3
            | StartsWith "4" | StartsWith "four" -> Some 4
            | StartsWith "5" | StartsWith "five" -> Some 5
            | StartsWith "6" | StartsWith "six" -> Some 6
            | StartsWith "7" | StartsWith "seven" -> Some 7
            | StartsWith "8" | StartsWith "eight" -> Some 8
            | StartsWith "9" | StartsWith "nine" -> Some 9
            | _ -> None)

    File.ReadAllLines path
    |> Seq.map parseLine
    |> Seq.map (fun ns -> Seq.head ns * 10 + Seq.last ns)
    |> Seq.sum
    |> string

[<Solution("2023", "1", "Trebuchet?!")>]
let Solver = simpleFileHandler solveSilver solveGold