module Scaffold.Extensions

open System.Collections

module Seq =
    let toPair (coll: seq<'T>) =
        Seq.toList coll
        |> function
        | [a; b] -> a, b
        | _ -> failwith "Not a two-element sequence"

module Map =
    let getOrDefault k def map =
        match Map.tryFind k map with
        | Some n -> n
        | None -> def

    let update k def fn map =
        Map.change k (function Some n -> Some (fn n) | None -> Some def) map