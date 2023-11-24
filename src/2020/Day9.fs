module Year2020.Day9

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Collections

#nowarn "25"

let parse = File.ReadAllLines >> Seq.map int64 >> Seq.toList

let findVuln input =
    let rec recur nums =
        let preamble, next :: _ = List.splitAt 25 nums
        let set = Set preamble
        preamble
        |> List.map (fun n -> next - n)
        |> List.tryFind (fun n -> Set.contains n set)
        |> function
        | Some _ -> recur <| List.tail nums
        | None -> next
    recur input

let solveSilver = findVuln >> sprintf "%i"

let solveGold input =
    let vuln = findVuln input
    let rec recur (ks: Queue.Queue<int64>) total rest =
        if total = vuln then
            Queue.toSeq ks
            |> Seq.minmax
            |> function
            | Some (l, h) -> l + h
            | None -> failwith "Empty list"
        else if total > vuln then
            let head, ks = Queue.dequeue ks
            recur ks (total - head) rest
        else
            let head :: rest = rest
            recur (Queue.enqueue head ks) (total + head) rest
    recur Queue.empty 0 input
    |> sprintf "%i"


[<Solution("2020", "9", "Encoding Error")>]
let Solver = chainFileHandler parse solveSilver solveGold