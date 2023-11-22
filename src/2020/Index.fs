module Year2020.Index

let private handlers = Map [
    "1", ("Report Repair", Day1.Solver)
    "2", ("Password Philosophy", Day2.Solver)
    "8", ("Handheld Halting", Day8.Solver)
]

let Handler day args =
    match Map.tryFind day handlers with
    | Some (_, solver) -> solver args
    | None -> failwithf "Invalid day: %s" day