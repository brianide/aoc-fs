module Year2020.Index

let private handlers = Map [
    "1", ("Report Repair", Day1.Solver)
    "2", ("Password Philosophy", Day2.Solver)
    "3", ("Toboggan Trajectory", Day3.Solver)
    "4", ("Passport Processing", Day4.Solver)
    "8", ("Handheld Halting", Day8.Solver)
]

let Handler day =
    match Map.tryFind day handlers with
    | Some (_, solver) -> solver
    | None -> failwithf "Invalid day: %s" day