module Scaffold.Extensions

open System.Collections

module Seq =
    let toPair (coll: seq<'T>) =
        Seq.toList coll
        |> function
        | [a; b] -> a, b
        | _ -> failwith "Not a two-element sequence"