module Year2023.Day2

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Patterns

#nowarn "25"

let parse path =
    let parseRound (line: string) =
        line.Split(", ")
        |> Seq.map _.Split(' ')
        |> Seq.map (fun [| Int32 num; color |] -> color, num)
        
    let parseGame (line: string) =
        let [| id; game |] = line.Split(": ")
        let rounds = game.Split("; ") |> Seq.map parseRound
        let id = id.Substring(5) |> int
        id, rounds

    File.ReadAllLines path
    |> Seq.map parseGame

let solveSilver games =
    let limits = Map [
        "red", 12
        "green", 13
        "blue", 14
    ]
    let isRoundPossible counts =
        Seq.forall (fun (color, count) -> Map.find color limits >= count) counts
    let isGamePossible (_, rounds) =
        Seq.forall isRoundPossible rounds

    Seq.filter isGamePossible games
    |> Seq.sumBy fst
    |> string

let solveGold games =
    let getGameMaxes (_, rounds) =
        Seq.concat rounds
        |> Seq.sortBy snd
        |> Map
    
    games
    |> Seq.sumBy (getGameMaxes >> Map.values >> Seq.reduce (*))
    |> string

[<Solution("2023", "2", "Cube Conundrum")>]
let Solver = chainFileHandler parse solveSilver solveGold