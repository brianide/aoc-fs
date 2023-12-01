module Scaffold.Extensions

open System.Collections
open System.Text.RegularExpressions

module Array2D  =

    let fold folder init arr =
        let mutable acc = init
        Array2D.iter (fun v -> acc <- folder acc v) arr
        acc

    let foldi folder init arr =
        let mutable acc = init
        Array2D.iteri (fun r c v -> acc <- folder acc r c v) arr
        acc

module String =

    let splitAt n (str: string) =
        let fore = str.Substring(0, n)
        let aft = str.Substring(n)
        (fore, aft)

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

    let partitionBy f (source: seq<_>) =
        use e = source.GetEnumerator()
        let mutable group = List.empty
        let mutable fv = None
        seq {
            while e.MoveNext() do
                match fv, f e.Current with
                | None, fcurr ->
                    fv <- Some fcurr
                    group <- e.Current :: group
                | Some p, fcurr when p = fcurr ->
                    group <- e.Current :: group
                | _, fcurr ->
                    yield List.rev group
                    group <- [e.Current]
                    fv <- Some fcurr
            
            yield List.rev group
        }

module Map =
    let getOrDefault k def map =
        match Map.tryFind k map with
        | Some n -> n
        | None -> def

    let update k def fn map =
        Map.change k (function Some n -> Some (fn n) | None -> Some def) map

type Regex with
    member reg.MatchSeq = reg.Matches >> Seq.map _.Value