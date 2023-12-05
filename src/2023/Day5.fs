module Year2023.Day5

open System.IO
open FParsec
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Patterns

#nowarn "25"

let parse =
    let letters = manySatisfy (isLetter)
    let seeds = skipString "seeds:" >>. manyTill (skipChar ' ' >>. pint64) (newline .>> newline)
    let mapHead = tuple2 (letters .>> skipString "-to-") (letters .>> skipString " map:" .>> newline)
    let mapRow = tuple3 (pint64 .>> spaces) (pint64 .>> spaces) (pint64 .>> spaces)
    let mapEntry = mapHead >>. (many mapRow)
    let spec = seeds .>>. many mapEntry

    File.ReadAllText
    >> run spec
    >> function
    | Success (res, _, _) -> res
    | Failure (msg, _, _) -> failwith msg

let solveSilver (seeds, mappings) =
    let resolveMappings mappings seed =
        let appliesTo value (_, src, len) =
            let diff = value - src
            diff >= 0L && diff < len

        let folder value map =
            Seq.tryFind (appliesTo value) map
            |> function
            | Some (dst, src, _) -> value - src + dst
            | None -> value

        Seq.fold folder seed mappings

    seeds
    |> Seq.map (resolveMappings mappings)
    |> Seq.min
    |> string

let toClosedRange start len =
    start, start + len - 1L

let overlap (lo, hi) (lo', hi') =
    if lo > hi' || hi < lo' then
        // No overlap
        None
    else if lo <= lo' && hi >= hi' then
        // First encompasses second
        Some (lo', hi')
    else if lo' < lo && hi' > hi then
        // Second encompasses first
        Some (lo, hi)
    else if hi < hi' then
        // First straddles second start
        Some (lo', hi)
    else if lo > lo' then
        // First straddles second end
        Some (lo, hi')
    else
        failwithf "Unhandled overlap scenario: %A" ((lo, hi), (lo', hi'))

let solveGold (ranges, mappings) =
    let mapRange range (dst, src, mapLen) =
        let srcRange = toClosedRange src mapLen
        overlap range srcRange
        |> Option.map (fun (lo, hi) -> lo - src + dst, hi - src + dst)

    let folder ranges map =
        seq {
            for range in ranges do
                for mapping in map do
                    match mapRange range mapping with
                    | Some n -> yield n
                    | None -> ()
        }

    let fillGaps map =
        let getGapFill ((_, src, mapLen), (_, src', _)) =
            let interSrc = src + mapLen
            let interLen = src' - interSrc
            if interSrc < src' then
                Some (interSrc, interSrc, interLen)
            else
                None

        map
        |> fun n -> (-1L, -1L, 0L) :: (System.Int64.MaxValue, System.Int64.MaxValue, 0L) :: n
        |> Seq.sortBy (fun (_, src, _) -> src)
        |> Seq.pairwise
        |> Seq.choose getGapFill
        |> fun f -> Seq.concat [f; map]

    Seq.chunkBySize 2 ranges
    |> Seq.map (Seq.toPair >> fun (st, len) -> toClosedRange st len) 
    |> fun ranges -> Seq.fold folder ranges (Seq.map fillGaps mappings)
    |> Seq.map fst
    |> Seq.min
    |> string

[<Solution("2023", "5", "If You Give A Seed A Fertilizer")>]
let Solver = chainFileHandler parse solveSilver solveGold