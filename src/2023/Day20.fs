module Year2023.Day20

open System.IO
open FParsec
open Scaffold.Parsec
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Collections

module Spec =
    type ModuleKind = FlipFlop | Conjunction | Broadcaster

module Sim = 
    type FlipFlop = { mutable On: bool }
    type Conjunction = { mutable Memory: Map<string,bool> }

    type Module =
    | FlipFlop of FlipFlop
    | Conjunction of Conjunction
    | Broadcaster
    | Untyped

let parse =
    let letters = many1Chars letter
    let flop = skipChar '%' >>. letters |>> fun s -> Spec.FlipFlop, s
    let conj = skipChar '&' >>. letters |>> fun s -> Spec.Conjunction, s
    let bcast = pstring "broadcaster" |>> fun s -> Spec.Broadcaster, s
    let dests = sepBy1 letters (skipString ", ")
    let line =
        (flop <|> conj <|> bcast) .>> skipString " -> " .>>. dests .>> newline
        |>> fun ((kind, name), dests) -> name, (kind, dests)

    File.ReadAllText >> getParsed (many1 line |>> Map.ofList)

let solveSilver conf =
    let sys = conf |> Map.map (fun _ (kind, dests) ->
        match kind with
        | Spec.FlipFlop -> Sim.FlipFlop { On = false }
        | Spec.Conjunction -> Sim.Conjunction { Memory = Map.empty }
        | Spec.Broadcaster -> Sim.Broadcaster
        |> fun m -> m, dests)

    let inline getModule key =
        match Map.tryFind key sys with
        | Some m -> m
        | None -> Sim.Untyped, []

    // Initialize conjunction connections
    for src, (_, dests) in Map.toList sys do
        for dst in dests do
            match getModule dst |> fst with
            | Sim.Broadcaster | Sim.Untyped | Sim.FlipFlop _ -> ()
            | Sim.Conjunction m -> m.Memory <- Map.add src false m.Memory

    let pushButton = ("button", "broadcaster", false)
    let mutable taps = Map.empty

    let rec pulse queue rem (lo, hi) =
        if Queue.isEmpty queue && rem = 0 then
            lo * hi
        else if Queue.isEmpty queue then
            pulse (Queue.enqueue pushButton queue) (rem - 1) (lo, hi)
        else
            let (source, key, high), queue = Queue.dequeue queue
            let lo, hi = if high then lo, hi + 1 else lo + 1, hi

            if high && key = "ll" then
                let cycle = 100000 - rem
                taps <- Map.update source [cycle] (fun cs -> cycle :: cs) taps

            let modu, dests = getModule key
            match modu with
            | Sim.Untyped -> []
            | Sim.Broadcaster ->
                List.map (fun s -> key, s, false) dests
            | Sim.FlipFlop m ->
                if not high then
                    m.On <- not m.On
                    List.map (fun s -> key, s, m.On) dests
                else []
            | Sim.Conjunction m ->
                m.Memory <- Map.add source high m.Memory
                let allHigh = Map.values m.Memory |> Seq.forall id
                List.map (fun s -> key, s, not allHigh) dests
            |> fun es -> pulse (Queue.enqueueAll es queue) rem (lo, hi)

    pulse Queue.empty 100000 (0, 0) |> printf "%d"

    Map.iter (fun _ v -> List.pairwise v |> List.map (fun (a, b) -> a - b) |> printfn "%A") taps

    ""

let solveGold input =
    "Not implemented"

[<Solution("2023", "20", "Pulse Propagation")>]
let Solver = chainFileHandler parse solveSilver solveGold