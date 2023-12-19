module Year2023.Day19

open System.IO
open FParsec
open Scaffold.Parsec
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Patterns

type Comp =
| GreaterThan of char * int
| LessThan of char * int
| Always

let parse =
    let flowName = many1Chars letter
    let cond =
        pipe4
            (anyOf "xmas") (anyOf "<>") pint32 (skipChar ':' >>. many1Chars letter)
            (fun key cmp v dst ->
                match cmp with
                | '<' -> LessThan (key, v), dst
                | '>' -> GreaterThan (key, v), dst
                | x -> failwithf "Invalid op: %A" x)

    let final = many1Chars letter |>> fun lbl -> Always, lbl

    let conds = sepBy1 (attempt cond <|> final) (skipChar ',')
    let workflow = tuple2 flowName (between (skipChar '{') (skipChar '}') conds)
    let workflows = many1 (workflow .>> newline) |>> Map.ofList

    let part = sepBy1 (anyOf "xmas" .>> skipChar '=' .>>. pint32) (skipChar ',') |>> Map.ofList
    let parts = many1 (between (skipChar '{') (skipChar '}') part .>> newline)

    let spec = workflows .>> newline .>>. parts

    File.ReadAllText
    >> getParsed spec


module Silver =
    let checkPart preds part =
        let rec recur part = function
        | "A" -> Map.values part |> Seq.sum
        | "R" -> 0
        | key ->
            Map.find key preds
            |> List.pick (fun (pred, dest) -> if pred part then Some dest else None)
            |> recur part
        recur part "in"

    let solve (flows, parts) =
        let predFor = function
        | GreaterThan (ch, v) -> fun part -> Map.find ch part > v
        | LessThan (ch, v) -> fun part -> Map.find ch part < v
        | Always -> fun _ -> true

        let preds = Map.map (fun _ flow -> List.map (fun (cmp, dst) -> predFor cmp, dst) flow) flows
        
        List.sumBy (checkPart preds) parts
        |> sprintf "%d"


module Gold =    
    let countPaths flows =
        let ranges = "xmas" |> Seq.map (fun ch -> ch, (1, 4000)) |> Map.ofSeq
        let splitGreater k n ranges =
            Map.alter k (fun (_, hi) -> n + 1, hi) ranges, Map.alter k (fun (lo, _) -> lo, n) ranges
        let splitLess k n ranges =
            Map.alter k (fun (lo, _) -> lo, n - 1) ranges, Map.alter k (fun (_, hi) -> n, hi) ranges
        
        let checkRanges = Map.forall (fun _ (lo, hi) -> lo < hi)

        let rec recur ranges = function
        | "A" ->
            Map.values ranges |> Seq.fold (fun prod (lo, hi) -> int64 (hi - lo + 1) * prod) 1L
        | "R" ->
            0L
        | flowName ->
            Map.find flowName flows
            |> List.fold (fun (total, ranges) flow ->
                if checkRanges ranges = false then
                    total, ranges
                else
                    match flow with 
                    | GreaterThan (ch, v), dst ->
                        let trimmed, remaining = splitGreater ch v ranges
                        total + (recur trimmed dst), remaining
                    | LessThan (ch, v), dst ->
                        let trimmed, remaining = splitLess ch v ranges
                        total + (recur trimmed dst), remaining
                    | Always, dst -> total + (recur ranges dst), ranges) (0L, ranges)
            |> fst

        recur ranges "in"

    let solve = fst >> countPaths >> sprintf "%d"

[<Solution("2023", "19", "Aplenty")>]
let Solver = chainFileHandler parse Silver.solve Gold.solve