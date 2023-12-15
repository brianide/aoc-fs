module Year2023.Day15

open System.IO
open FParsec
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Parsec

let hash = Seq.fold (fun acc ch -> (acc + int ch) * 17 % 256) 0

let solveSilver =
    File.ReadAllLines
    >> String.concat ""
    >> _.Split(",")
    >> Seq.sumBy hash
    >> sprintf "%d"

module Gold =
    type Action = 
    | Dash of string
    | Equal of string * int

    type Lens = { Label: string; mutable Length: int }

    let parse =
        let dash = many1CharsTill letter (skipChar '-') |>> Dash
        let equal = many1CharsTill letter (skipChar '=') .>>. pint32 |>> Equal
        let spec = sepBy1 (attempt dash <|> equal) (skipChar ',')

        File.ReadAllText
        >> getParsed spec

    let solve path =
        let folder (acc: Map<int,list<Lens>>) = function
        | Dash lbl ->
            let update = function
            | Some box -> List.filter (fun n -> n.Label <> lbl) box |> Some
            | None -> None
            Map.change (hash lbl) update acc
        | Equal (lbl, foc) ->
            let update = function
            | Some box ->
                List.tryFind (fun n -> n.Label = lbl) box
                |> function
                | Some lens -> lens.Length <- foc; box
                | None -> { Label = lbl; Length = foc } :: box
                |> Some
            | None ->
                Some [{ Label = lbl; Length = foc }]
            Map.change (hash lbl) update acc

        parse path
        |> List.fold folder Map.empty
        |> Map.toList
        |> Seq.sumBy (fun (i, ls) -> ls |> Seq.rev |> Seq.mapi (fun slot lens -> (i + 1) * (slot + 1) * lens.Length) |> Seq.sum)
        |> sprintf "%d"

[<Solution("2023", "15", "Lens Library")>]
let Solver = simpleFileHandler solveSilver Gold.solve