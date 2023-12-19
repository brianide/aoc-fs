module Year2023.Day19

open System.IO
open FParsec
open Scaffold.Parsec
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Patterns

let parse =
    let flowName = many1Chars letter
    let cond =
        pipe4
            (anyOf "xmas") (anyOf "<>") pint32 (skipChar ':' >>. many1Chars letter)
            (fun k op v dest ->
                let comp = match op with '<' -> (<) | '>' -> (>) | x -> failwithf "Invalid op: %A" x
                (fun obj -> comp (Map.find k obj) v), dest)

    let final = many1Chars letter |>> (fun dest -> (fun _ -> true), dest)
    let conds = sepBy1 (attempt cond <|> final) (skipChar ',')
    let workflow = tuple2 flowName (between (skipChar '{') (skipChar '}') conds)
    let workflows = many1 (workflow .>> newline) |>> Map.ofList

    let object = sepBy1 (anyOf "xmas" .>> skipChar '=' .>>. pint32) (skipChar ',') |>> Map.ofList
    let objects = many1 (between (skipChar '{') (skipChar '}') object .>> newline)

    let spec = workflows .>> newline .>>. objects

    File.ReadAllText
    >> getParsed spec

let checkPart flows part =
    let rec recur part = function
    | "A" -> Map.values part |> Seq.sum
    | "R" -> 0
    | key ->
        Map.find key flows
        |> List.pick (fun (pred, dest) -> if pred part then Some dest else None)
        |> recur part
    recur part "in"

let solveSilver (flows, parts) =
    List.sumBy (checkPart flows) parts
    |> sprintf "%d"

let solveGold input =
    "Not implemented"

[<Solution("2023", "19", "Aplenty")>]
let Solver = chainFileHandler parse solveSilver solveGold