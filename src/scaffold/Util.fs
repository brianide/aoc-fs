module Scaffold.Util

open System.Text.RegularExpressions

module Inspect =
    let inline tap f v = f v; v
    let inline tapIter f v = Seq.iter f v; v


module Patterns =
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