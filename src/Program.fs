open System.IO
open System.Diagnostics

let private years = Map [
    "2020", Year2020.Index.Handler
]

[<EntryPoint>]
let main args =
    match Array.toList args with
    | [] -> 
        printfn "Usage: aoc [year] [day] [part] [file]"
    | year :: day :: rest ->
        match Map.tryFind year years with
        | Some handler ->
            let daySolver = handler day
            let watch = Stopwatch()
            watch.Start()
            let res = daySolver rest
            watch.Stop()
            $"{res}\n\nRan in {watch.ElapsedMilliseconds}ms"
        | None -> failwithf "Invalid year: %s" year
        |> printfn "%s"
    | _ -> failwith "Invalid args"
    0