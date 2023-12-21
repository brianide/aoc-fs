module Year2023.Day20

open System.IO
open FParsec
open Scaffold.Parsec
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Collections

type FlipFlop = { mutable On: bool }
type Conjunction = { mutable Memory: Map<string,bool> }
type Module =
| FlipFlop of FlipFlop
| Conjunction of Conjunction
| Broadcaster
| Untyped

let parse =
    let letters = many1Chars letter
    let flop = skipChar '%' >>. letters |>> fun s -> FlipFlop { On = false }, s
    let conj = skipChar '&' >>. letters |>> fun s -> Conjunction { Memory = Map.empty }, s
    let bcast = pstring "broadcaster" |>> fun s -> Broadcaster, s
    let dests = sepBy1 letters (skipString ", ")
    let lines =
        (flop <|> conj <|> bcast) .>> skipString " -> " .>>. dests .>> newline
        |>> fun ((kind, name), dests) -> name, (kind, dests)
        |> many1
        |>> Map.ofList

    File.ReadAllText
    >> getParsed lines

let solveSilver sys =
    let inline getModule key =
        match Map.tryFind key sys with
        | Some m -> m
        | None -> Untyped, []

    // Initialize conjunction connections
    for src, (_, dests) in Map.toList sys do
        for dst in dests do
            match getModule dst |> fst with
            | Broadcaster | Untyped | FlipFlop _ -> ()
            | Conjunction m -> m.Memory <- Map.add src false m.Memory

    let pushButton = ("button", "broadcaster", false)

    let rec pulse queue rem (lo, hi) =
        if Queue.isEmpty queue && rem = 0 then
            lo * hi
        else if Queue.isEmpty queue then
            pulse (Queue.enqueue pushButton queue) (rem - 1) (lo, hi)
        else
            let (source, key, high), queue = Queue.dequeue queue
            let lo, hi = if high then lo, hi + 1 else lo + 1, hi

            let modu, dests = getModule key
            match modu with
            | Untyped -> []
            | Broadcaster ->
                List.map (fun s -> key, s, false) dests
            | FlipFlop m ->
                if not high then
                    m.On <- not m.On
                    List.map (fun s -> key, s, m.On) dests
                else []
            | Conjunction m ->
                m.Memory <- Map.add source high m.Memory
                let allHigh = Map.values m.Memory |> Seq.forall id
                List.map (fun s -> key, s, not allHigh) dests
            |> fun es -> pulse (Queue.enqueueAll es queue) rem (lo, hi)

    pulse Queue.empty 1000 (0, 0)
    |> sprintf "%d"

[<Solution("2023", "20", "Pulse Propagation")>]
let Solver = stubFileHandler (parse >> solveSilver)