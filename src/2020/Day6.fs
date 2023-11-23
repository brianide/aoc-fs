module Year2020.Day6

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers

let solveSilver path =
    let rec recur total seen = function
    | '\n' :: '\n' :: rest ->
        let total = total + Seq.length seen
        recur total Set.empty rest
    | '\n' :: rest ->
        recur total seen rest
    | c :: rest ->
        let seen = Set.add c seen
        recur total seen rest
    | [] ->
        total + Seq.length seen

    File.ReadAllText path
    |> Seq.toList
    |> recur 0 Set.empty
    |> sprintf "%A"

let solveGold path =
    let rec groupify groups curr = function
    | [] :: rest ->
        groupify (curr :: groups) [] rest
    | line :: rest ->
        groupify groups (Set line :: curr) rest
    | [] ->
        curr :: groups

    File.ReadAllLines path
    |> Seq.toList
    |> List.map Seq.toList
    |> groupify [] []
    |> List.sumBy (Set.intersectMany >> Seq.length)
    |> sprintf "%A"

[<Solution("2020", "6", "Custom Customs")>]
let Solver = simpleFileHandler solveSilver solveGold