module Year2023.Day19

open System.IO
open FParsec
open Scaffold.Parsec
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Patterns

module Silver =
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

    let solve path =
        let (flows, parts) = parse path
        List.sumBy (checkPart flows) parts
        |> sprintf "%d"

module Gold =
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

        let object = sepBy1 (anyOf "xmas" .>> skipChar '=' .>>. pint32) (skipChar ',') |>> Map.ofList
        let objects = many1 (between (skipChar '{') (skipChar '}') object .>> newline)

        let spec = workflows .>> newline .>>. objects

        File.ReadAllText
        >> getParsed spec
    
    let countPaths flows =
        let obj = "xmas" |> Seq.map (fun ch -> ch, (1, 4000)) |> Map.ofSeq
        let trimGreater min (lo, hi) = (min + 1 |> max lo), hi
        let trimLess max (lo, hi) = lo, (max - 1 |> min hi)
        let checkRanges = Map.forall (fun _ (lo, hi) -> lo < hi)

        let rec recur obj = function
        | "A" ->
            Map.values obj |> Seq.fold (fun prod (lo, hi) -> int64 (hi - lo + 1) * prod) 1L
        | "R" ->
            0L
        | flowName ->
            Map.find flowName flows
            |> List.sumBy (
                function
                | GreaterThan (ch, cap), dst ->
                    let trimmed = Map.alter ch (trimGreater cap) obj
                    if checkRanges trimmed then
                        recur trimmed dst
                    else
                        0
                | LessThan (ch, bot), dst ->
                    let trimmed = Map.alter ch (trimLess bot) obj
                    if checkRanges trimmed then
                        recur trimmed dst
                    else
                        0
                | Always, dst -> recur obj dst)

        recur obj "in"

    let solve path =
        let flows, _ = parse path
        countPaths flows
        |> sprintf "%d"

[<Solution("2023", "19", "Aplenty")>]
let Solver = simpleFileHandler Silver.solve Gold.solve