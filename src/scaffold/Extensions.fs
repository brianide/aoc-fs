module Scaffold.Extensions

open System.Collections
open System.Text.RegularExpressions

module Array2D  =

    let inline dimensions arr = Array2D.length1 arr, Array2D.length2 arr

    let isInside r c arr = 0 <= r && r < Array2D.length1 arr && 0 <= c && c < Array2D.length2 arr

    let indices arr = seq { for r in 0 .. Array2D.length1 arr - 1 do for c in 0 .. Array2D.length2 arr - 1 do r, c }

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
    /// Splits the sequence into two lists according to the given predicate function.
    /// Returns a tuple of two lists containing the sequence elements for which the
    /// predicate returns true and false, respectively.
    let splitWith pred (coll: seq<_>) =
        let mutable tru = []
        let mutable fal = []
        use enum = coll.GetEnumerator()
        while enum.MoveNext() do
            if pred enum.Current then
                tru <- enum.Current :: tru
            else
                fal <- enum.Current :: fal
        List.rev tru, List.rev fal

    let rec tails col = seq {
        if Seq.isEmpty col |> not then
            yield col
            yield! tails (Seq.tail col)
    }

    let join (delim: 'a) (colls: seq<seq<'a>>) =
        let mutable first = true
        seq {
            for items in colls do
                yield! items
                if first then
                    first <- false
                else
                    yield delim
        }


    let frequencies (coll: seq<_>) =
        Seq.groupBy id coll
        |> Seq.map (fun (i, is) -> i, Seq.length is)

    let takeUpTo lim (coll: seq<_>) =
        use enum = coll.GetEnumerator()
        let mutable n = lim
        seq {
            while n > 0 && enum.MoveNext() do
                yield enum.Current
                n <- n - 1
        }

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

    let alter k fn map =
        Map.change k (function Some n -> Some (fn n) | None -> failwithf "Key not found: %A" k) map

type Regex with
    member reg.MatchSeq = reg.Matches >> Seq.map _.Value