module Year2020.Day16

open System.IO
open System.Text.RegularExpressions
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Patterns

let parse path =
    let parseFields text =
        Regex(@"^(.+): (\d+)-(\d+) or (\d+)-(\d+)$", RegexOptions.Multiline)
        |> _.GroupsSeq(text)
        |> Seq.map (function
        | [name; Int32 alo; Int32 ahi; Int32 blo; Int32 bhi] -> (name, (alo, ahi), (blo, bhi))
        | x -> failwithf "Invalid match: %A" x)
        |> Seq.toArray
    
    let parseTickets (text: string) =
        text.Split('\n')
        |> Seq.skip 1
        |> Seq.filter (fun s -> s.Length > 0)
        |> Seq.map (_.Split(',') >> Seq.map int)

    File.ReadAllText path |> _.Split("\n\n")
    |> function
    | [| fieldSpecs; myTicket; nearTickets |] ->
        (parseFields fieldSpecs, parseTickets myTicket |> Seq.exactlyOne, parseTickets nearTickets)
    | x -> failwithf "Invalid segments: %A" x

let inRange (lo, hi) a = lo <= a && a <= hi

let matchesSpec (_, a, b) n = inRange a n || inRange b n 
let isValidField fieldSpecs n = Seq.exists (fun spec -> matchesSpec spec n) fieldSpecs

let solveSilver (fieldSpecs, _, tickets) =
    Seq.collect id tickets
    |> Seq.filter (isValidField fieldSpecs >> not)
    |> Seq.sum
    |> string

// class: 0-1 or 4-19
// row: 0-5 or 8-19
// seat: 0-13 or 16-19

// your ticket:
// 11,12,13

// nearby tickets:
// 3,9,18
// 15,1,5
// 5,14,9

let solveGold (fieldSpecs, myTicket, tickets) =
    let validTickets = Seq.filter (Seq.forall (isValidField fieldSpecs)) tickets
    let columns =
        validTickets
        |> Seq.map (Seq.map (fun field ->
            fieldSpecs
            |> Seq.indexed
            |> Seq.choose (fun (i, spec) ->
                if matchesSpec spec field then
                    Some i
                else
                    None)
            |> set))
        |> Seq.transpose
        |> Seq.indexed

    let rec solveColumns unsolved assoc =
        let newSolves =
            unsolved
            |> Seq.map (fun (i, col) -> i, Set.intersectMany col)
            |> Seq.choose (fun (i, join) -> if Set.count join = 1 then Some (i, Seq.exactlyOne join) else None)
        
        let unsolved = seq {
            for (i, col) in unsolved do
                yield i, Seq.fold (fun acc (_, spec) -> Set.remove spec acc) col newSolves
        }

        printfn "%A" unsolved
                // |> Seq.fold (fun set ->
                //     let (name, _, _) = fieldSpecs[Seq.exactlyOne set]
                //     let assoc = Map.add name i assoc
                //     let unsolved = unsolved |> Seq.map (fun (j, )))

    solveColumns columns Map.empty
    ""

[<Solution("2020", "16", "Ticket Translation")>]
let Solver = chainFileHandler parse solveSilver solveGold