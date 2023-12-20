module Scaffold.Util

open System.Text.RegularExpressions

module Inspect =
    let inline tap f v = f v; v
    let inline tapIter f v = Seq.iter f v; v

module Search =
    let inline binarySearch fn target min max =
        let one = LanguagePrimitives.GenericOne
        let two = one + one
        let rec helper l r =
            if l = r then
                l
            else
                let m = (l + r) / two
                let a = fn m
                if a <= target then
                    helper (m + one) r
                else
                    helper l m
        helper min max

module Patterns =
    let (|TryParse|_|) fn (x: string) =
        try fn x |> Some with :? System.FormatException -> None

    let (|Int32|_|) (x: string) =
        try int x |> Some with :? System.FormatException -> None

    let (|Int64|_|) (x: string) =
        try int64 x |> Some with :? System.FormatException -> None
    
    let (|Char|_|) (x: string) =
        try char x |> Some with :? System.FormatException -> None

    let (|RegGroups|_|) patt str =
        let reg = Regex(patt)
        let mat = reg.Match(str)
        match mat.Success, mat.Groups with
        | true, groups -> Seq.tail groups |> Seq.map (fun n -> n.Value) |> Seq.toList |> Some
        | false, _ -> None

module Collections =

    module Queue =
        type Queue<'a> = Queue of 'a list * 'a list

        let empty = Queue ([], [])

        let singleton e = Queue ([], [e])

        let isEmpty = function Queue ([], []) -> true | _ -> false

        let length = function Queue (ins, outs) -> List.length ins + List.length outs

        let contains e = function
            | Queue (ins, outs) -> List.contains e ins || List.contains e outs

        let head = function
            | Queue ([], []) -> failwith "No elements remaining"
            | Queue (ins, []) -> List.last ins
            | Queue (_, outs) -> List.head outs

        let enqueue e q =
            match q with Queue (ins, outs) -> Queue(e :: ins, outs)
        
        let enqueueAll es q =
            match q with Queue (ins, outs) -> Queue(List.concat [List.rev es; ins], outs)

        let dequeue = function
            | Queue ([], []) -> failwith "No elements remaining"
            | Queue (ins, e :: outs) -> (e, Queue (ins, outs))
            | Queue (ins, []) ->
                let outs = List.rev ins
                (List.head outs, Queue ([], List.tail outs))

        let toSeq = function
            | Queue (ins, outs) -> Seq.append outs (List.rev ins)

        let ofList s = Queue ([], s)