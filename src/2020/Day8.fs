module Year2020.Day8

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Util.Patterns

type Instruction =
| Nop of int
| Acc of int
| Jmp of int

let parse path =
    let parseLine line =
        match (line: string).Split(" ") with
        | [| "nop"; Int32 n |] -> Nop n
        | [| "acc"; Int32 n |] -> Acc n
        | [| "jmp"; Int32 n |] -> Jmp n
        | _ -> failwithf "Invalid input: %s" line

    File.ReadAllLines path
    |> Array.map parseLine

let solveSilver input =
    let rec sim seen acc ip (prog: Instruction array) =
        if Set.contains ip seen then
            acc
        else
            let seen = Set.add ip seen
            match prog[ip] with
            | Acc n -> sim seen (acc + n) (ip + 1) prog
            | Jmp n -> sim seen acc (ip + n) prog
            | Nop _ -> sim seen acc (ip + 1) prog
    
    sim Set.empty 0 0 input
    |> sprintf "%A"

let solveGold input =
    let rec sim seen acc ip patched (prog: Instruction array) =
        if ip = prog.Length then
            Some acc
        else if Set.contains ip seen then
            None
        else
            let seen = Set.add ip seen
            match prog[ip] with
            | Acc n -> sim seen (acc + n) (ip + 1) patched prog
            | Jmp n ->
                if patched then
                    sim seen acc (ip + n) true prog
                else
                    match sim seen acc (ip + 1) true prog with
                    | Some n -> Some n
                    | None -> sim seen acc (ip + n) false prog
            | Nop n ->
                if patched then
                    sim seen acc (ip + 1) true prog
                else
                    match sim seen acc (ip + n) true prog with
                    | Some n -> Some n
                    | None -> sim seen acc (ip + 1) false prog

    sim Set.empty 0 0 false input
    |> function Some n -> n | None -> failwith "Program error"
    |> sprintf "%A"

[<Solution("2020", "8", "Handheld Halting")>]
let Solver = chainFileHandler parse solveSilver solveGold