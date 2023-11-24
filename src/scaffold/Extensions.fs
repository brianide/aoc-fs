module Scaffold.Extensions

open System.Collections

module Seq =
    let toPair (coll: seq<'T>) =
        Seq.toList coll
        |> function
        | [a; b] -> a, b
        | _ -> failwith "Not a two-element sequence"

    let minmax (coll: seq<'T>) =
        let folder acc e =
            match acc with
            | Some (l, h) when e < l -> Some (e, h)
            | Some (l, h) when e > h -> Some (l, e)
            | Some r -> Some r
            | None -> Some (e, e)

        Seq.fold folder None coll

module Map =
    let getOrDefault k def map =
        match Map.tryFind k map with
        | Some n -> n
        | None -> def

    let update k def fn map =
        Map.change k (function Some n -> Some (fn n) | None -> Some def) map