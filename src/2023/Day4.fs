module Year2023.Day4

open System.IO
open FParsec
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions

let parse path =
    let cardNum = skipString "Card" >>. spaces1 >>. pint32 .>> skipChar ':' .>> spaces1
    let ws = skipMany (pchar ' ')
    let numList term = many1Till (pint32 .>> ws) term |>> set
    let line = cardNum .>>. (numList (skipChar '|' .>> spaces1) .>>. numList newline)

    File.ReadAllText path
    |> run (many line)
    |> function
    | Success (res, _, _) -> res
    | Failure (msg, _, _) -> failwith msg

let countWinning (a, b) =
    Set.intersect a b |> Seq.length

let solveSilver =
    Seq.sumBy (snd >> countWinning >> (fun n -> n - 1) >> pown 2)
    >> string

let solveGold input =
    let cards = input |> Seq.map (snd >> countWinning) |> Seq.rev |> Seq.toList
    let rec proc prev = function
    | [] -> Seq.sum prev
    | next :: rest -> prev |> Seq.takeUpTo next |> Seq.sum |> ((+) 1) |> fun n -> proc (n :: prev) rest

    proc [] cards
    |> string

[<Solution("2023", "4", "Scratchcards")>]
let Solver = chainFileHandler parse solveSilver solveGold