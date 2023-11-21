module Scaffold.Handlers

let private (|Part|_|) silver gold = function
| "s" | "silver" -> Some [silver]
| "g" | "gold" -> Some [gold]
| "b" | "both" -> Some [silver; gold]
| _ -> None

/// Creates a simple handler where only the first part of the problem is implemented, with no
/// distinct step for parsing.
let stubFileHandler solve = function
| ["s"; path] -> solve path
| _ -> failwith "Invalid input"

/// Creates a handler for solutions where both parts receive identical input from a common parsing
/// function.
let chainFileHandler (parse: string -> 'a) (silver: 'a -> string) (gold: 'a -> string) = function
| [Part silver gold p; path] ->
    let data = parse path
    List.map (fun f -> f data) p
    |> String.concat "\n"
| _ -> failwith "Invalid input"