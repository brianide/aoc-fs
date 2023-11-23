open System.IO
open System.Diagnostics

open System.Text.RegularExpressions

open System
open Scaffold.Attributes

let solutions =
    Reflection.Assembly.GetExecutingAssembly()
    |> _.GetTypes()
    |> Seq.collect _.GetProperties()
    |> Seq.map (fun prop -> prop, Attribute.GetCustomAttribute(prop, typeof<SolutionAttribute>, false) |> Option.ofObj)
    |> Seq.choose (function prop, Some attr -> Some (prop.GetMethod.Invoke(null, null) :?> string list -> string, attr :?> SolutionAttribute) | _, None -> None)
    |> Seq.sortBy (fun (_, attr) -> attr.Year, attr.Id)
    |> Seq.toList

let usage =
    solutions
    |> List.map (fun (_, attr) -> sprintf "* %4s %3s - %s" attr.Year attr.Id attr.Name)
    |> String.concat "\n"
    |> sprintf "Usage: aoc [year] [day] [part] [file]\n\n%s"

[<EntryPoint>]
let main args =
    match Array.toList args with
    | year :: day :: rest ->
        solutions
        |> List.tryFind (fun (_, attr) -> attr.Year = year && attr.Id = day)
        |> function
        | Some (fn, attr) ->
            let watch = Stopwatch()
            printfn "Running \"%s\"" attr.Name

            watch.Start()
            let res = fn rest
            watch.Stop()

            $"{res}\n\nRan in {watch.ElapsedMilliseconds}ms"
        | None -> failwithf "Invalid solution specified\n\n%s" usage
        |> printfn "%s"
        0
    | _ -> 
        failwith usage